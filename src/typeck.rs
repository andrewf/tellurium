// use parsetree::*;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use parsetree;
use parsetree::Expression;
use parsetree::Expression::*;
use parsetree::Statement;
use parsetree::Statement::*;
use parsetree::FunDef;
use parsetree::VarDef;
use parsetree::ExternDef;
use parsetree::TopLevelItem;
use platform::Platform;
use flowgraph::*;
use common::*;

pub type GlobalVarScope = HashMap<String, DataType>;

pub struct LocalScope<'a> {
    globals: &'a GlobalVarScope,
    locals: HashMap<String, usize>, // string -> slot
}

impl<'a> LocalScope<'a> {
    pub fn new(glob: &'a GlobalVarScope) -> Self {
        LocalScope {
            globals: glob,
            locals: HashMap::new(),
        }
    }
    pub fn put_raw(&mut self, name: &String, slot: usize) {
        self.locals.insert(name.clone(), slot);
    }
    pub fn put(&mut self, name: &String, slot: usize, graph: &mut FlowGraph) -> Result<(), Error> {
        // self.locals.insert(name.clone(), slot);
        match self.globals.get(name) {
            None => return mkerr("can only assign to existing globals"),
            Some(_dt) => {
                // create and push a CopyOnly node
                let mut reqs = HwReqs::new();
                reqs.push_before(HwLoc::labelled_var(&name).into());
                let n = Node {
                    action: NodeAction::CopyOnly,
                    inputs: vec![slot],
                    outputs: Vec::new(),
                    hwreqs: reqs,
                };
                graph.nodes.push(n);
                // TODO invalidate existing slots pointing to this global
                Ok(())
            }
        }
    }
    pub fn get(&self, name: &String, graph: &mut FlowGraph) -> Result<(usize, DataType), Error> {
        match self.locals.get(name) {
            Some(slot) => Ok((*slot, graph.localslots[*slot].clone())),
            None => {
                match self.globals.get(name) {
                    Some(ref dt) => {
                        // make a new graph node that introduces a new slot
                        // with the value of the global variable
                        let slot = graph.new_slot(dt);
                        let n = node_from_hw(HwLoc::labelled_var(&name), slot);
                        graph.nodes.push(n);
                        Ok((slot, (*dt).clone()))
                    }
                    None => mkerr(&format!("couldn't find var {}", name)),
                }
            }
        }
    }
}

fn extract_fun(general: (usize, DataType)) -> Result<(usize, FunSignature), Error> {
    let (slot, dt) = general;
    match dt {
        DataType::Composite(CompositeType::Fun(sig)) => Ok((slot, sig)),
        _ => Err(Error { msg: "not callable".into() }),
    }
}

// split all the toplevel items into separate vectors
fn demux_toplevel(tl: parsetree::TopLevel) -> (Vec<FunDef>, Vec<VarDef>, Vec<ExternDef>) {
    let mut globals = Vec::new();
    let mut functions = Vec::new();
    let mut externs = Vec::new();
    // get out vars, functions
    for item in tl.into_iter() {
        match item {
            TopLevelItem::VarDef(v) => {
                // ignore initial value for now?
                globals.push(v)
            }
            TopLevelItem::FunDef(f) => functions.push(f),
            TopLevelItem::ExternDef(e) => externs.push(e),
        }
    }
    (functions, globals, externs)
}

// typecheck the program in tl and return it in a graph-based
// format suitable for codegen
pub fn check_and_flowgen<P: Platform>(tl: parsetree::TopLevel,
                                      platform: &P)
                                      -> Result<CheckedProgram, Error> {
    let mut types: GlobalTypeNamespace = platform.get_basic_types();
    let (functions, vars, externs) = demux_toplevel(tl);
    // ignore vars for now
    // flow_graphs is a parallel array to functions
    // generate seperately so we can borrow functions while making it
    let flow_graphs = {
        let var_scope = try!(make_global_scopes(&functions, &vars, &externs));
        // use function_scope to build the graph for each function
        let mut flow_graphs = Vec::new();
        // generate body flow graphs
        for f in functions.iter() {
            flow_graphs.push(try!(flowgen_function(platform, &var_scope, &f)))
        }
        flow_graphs
    };

    let checked_functions = functions.into_iter()
                                     .zip(flow_graphs)
                                     .map(|(f, b)| {
                                         CheckedFunDef {
                                             ld_name: f.ld_name,
                                             signature: f.signature,
                                             body: b,
                                         }
                                     })
                                     .collect();

    return Ok(CheckedProgram {
        externs: externs.into_iter().map(|e| e.ld_name).collect(),
        function_definitions: checked_functions,
        global_vars: vars,
    });
}

// parses a list of FunDefs and ExternDefs into a hashmap
// so we can look up functions by name without a linear search
// through two data structures.
// also make global variable scope
// also ensure there are no duplicate names
fn make_global_scopes<'a>(functions: &'a Vec<FunDef>,
                          vars: &'a Vec<VarDef>,
                          externs: &'a Vec<ExternDef>)
                          -> Result<GlobalVarScope, Error> {
    let mut var_scope = GlobalVarScope::new();
    // build function scope
    // first put in extern declarations
    for ext in externs.iter() {
        match var_scope.entry(ext.ld_name.clone()) {
            Entry::Vacant(vac) => {
                vac.insert(ext.datatype.clone());
            }
            Entry::Occupied(_) => {
                return mkerr(&format!("duplicate name {}", ext.ld_name));
            }
        }
    }
    // put in Te-defined functions
    for fun in functions.iter() {
        match var_scope.entry(fun.ld_name.clone()) {
            Entry::Vacant(vac) => {
                vac.insert(DataType::Composite(CompositeType::Fun(fun.signature.clone())));
            }
            Entry::Occupied(_) => {
                return mkerr(&format!("duplicate function {}", fun.ld_name));
            }
        }
    }
    // Te-defined vars
    for v in vars.iter() {
        match var_scope.entry(v.ld_name.clone()) {
            Entry::Vacant(vac) => {
                vac.insert(v.datatype.clone());
            }
            _ => return mkerr(&format!("duplicate var {}", v.ld_name)),
        }
    }
    Ok(var_scope)
}

// current types, value scope, function scope
// useful in building a FlowGraph
// struct NameState<'a> {
//    graph: &'a FlowGraph,
//    global_functions: &'a FunctionScope<'a>,
//    global_vars: &'a GlobalVarScope,
//    local_vars: HashMap<String, (usize, &'a DataType)>
// }
//
// impl<'a> NameState<'a> {
//    fn new(g: &'a FlowGraph, f: &'a FunctionScope<'a>, v: &'a GlobalVarScope) -> Self {
//        NameState{
//            graph: g,
//            global_functions: f,
//            global_vars: v,
//            local_vars: HashMap::new()
//        }
//    }
//    fn set_name(&mut self, name: &String, slot: usize){
//        let t = graph.localslots.get(slot).unwrap();
//        local_vars.insert(name.clone(), (slot, &t))
//    }
//    fn get_name(&self, name: &String) -> Result<(NodeValue, &'a DataType), Error> {
//        match local_vars.get(name) {
//            Some((slot, t)) => {
//                Ok(Location::Slot(slot), t)
//            }
//            None => {
//                // check globals
//
//
//    }
// /    fn dispatch(&self, name: &String, sig: &FunSignature)
// /        -> Result<(Location, FunSignature), DispatchError> {
// /    }
// /    fn push_frame(&mut self) {}
// /    fn pop_frame(&mut self) {}
// }

fn flowgen_function<'a>(plat: &Platform,
                        var_scope: &'a GlobalVarScope,
                        fundef: &FunDef)
                        -> Result<FlowGraph, Error> {
    let mut graph = FlowGraph::new();
    let mut localscope = LocalScope::new(var_scope);
    // add slots for function args
    for (t, name) in fundef.signature.argtypes.iter().zip(fundef.argnames.iter()) {
        // TODO add to scope
        let index = graph.new_slot(t);
        graph.reqs.push_before(HwRange::new()); // todo calling convention
        localscope.put_raw(name, index);
    }
    // go through body and add statements
    for s in fundef.body.iter() {
        match s {
            &Statement::Expr(ref expr) => {
                try!(flowgen_expr(expr, plat, &mut graph, &mut localscope));
            }
            _ => unimplemented!(),
        }
    }
    // try!(flowgen_block(&mut namestate, &mut graph, fundef.body));
    Ok(graph)
}

pub fn flowgen_expr(expr: &Expression,
                    plat: &Platform,
                    graph: &mut FlowGraph,
                    locals: &mut LocalScope)
                    -> Result<Vec<usize>, Error> {
    match expr {
        &Literal(ref n) => {
            let slot = graph.new_slot(&DataType::Basic("i32".into()));
            let n = node_from_hw(HwLoc::Imm(n.clone()), slot);
            graph.nodes.push(n);
            Ok(vec![slot])
        }

        &Ident(ref s) => {
            let (slot, _dt) = try!(locals.get(s, graph));
            Ok(vec![slot])
        }
        &FunCall(box Ident(ref name), ref args) => {
            // normal function call
            // look up function
            let (callee_slot, sig) = try!(extract_fun(try!(locals.get(name, graph))));
            // just 0 or 1, for now
            let output_slots = match sig.return_type {
                box Some(ref ret) => vec![graph.new_slot(ret)],
                box None => Vec::new(),
            };
            // generate input slots
            let mut input_slots: Vec<usize> = {
                let input_results = args.iter()
                                        .map(|e| -> Result<usize, Error> {
                                            flowgen_expr(e, plat, graph, locals).and_then(|v| {
                                                v.into_iter()
                                                 .nth(0)
                                                 .ok_or(Error { msg: "arg is ()".into() })
                                            })
                                        });
                try!(input_results.collect())
            }; // borrow graph
            // slot for function itself is at end
            input_slots.push(callee_slot);
            // hardware reqs depend on platform
            let reqs = try!(plat.get_fun_hwreqs(&sig));
            // put finished call node in list
            graph.nodes.push(Node {
                action: NodeAction::Call(sig),
                inputs: input_slots,
                outputs: output_slots.clone(),
                hwreqs: reqs,
            });
            Ok(output_slots)
        }
        &Assign(box Ident(ref assignee), box ref val) => {
            // get slot of assigned value
            let r = try!(flowgen_expr(val, plat, graph, locals));
            if r.len() != 1 {
                return mkerr("can only assign one slot");
            }
            // put does work of creating a CopyOnly node
            try!(locals.put(assignee, r[0], graph));
            Ok(Vec::new())
        }
        _ => unimplemented!(),
    } // match
}

#[cfg(test)]
mod testy {
    use num::BigInt;
    use std::collections::HashMap;
    use std::collections::hash_map::Entry;
    use parsetree::Expression;
    use parsetree::Expression::*;
    use parsetree::Statement;
    use parsetree::Statement::*;
    use parsetree::FunDef;
    use parsetree::VarDef;
    use parsetree::ExternDef;
    use super::*;
    use common::*;
    use flowgraph::*;
    use codegen::IntelPlatform;

    fn test_setting() -> (IntelPlatform, FlowGraph, GlobalVarScope) {
        (IntelPlatform, FlowGraph::new(), GlobalVarScope::new())
    }

    // macro_rules! fixture {
    //    _ => {
    //        let plat = IntelPlatform;
    //        let mut graph = FlowGraph::new();
    //        let mut globals = GlobalVarScope::new();
    //        let mut locals = LocalScope::new(&globals);
    //    }
    // }

    #[test]
    fn literal() {
        let (plat, mut graph, globals) = test_setting();
        let mut locals = LocalScope::new(&globals);
        let r = flowgen_expr(&Expression::Literal(bigint(42)),
                             &plat,
                             &mut graph,
                             &mut locals);
        let newslots = r.expect("flowgen of a literal should not fail. that is dumb");
        let expected_slot = 0;
        assert!(newslots[0] == expected_slot);
        assert!(graph.localslots[expected_slot] == DataType::Basic("i32".into()));
    }

    #[test]
    fn funcall() {
        let intvoid: FunSignature = FunSignature {
            argtypes: vec![DataType::Basic("i32".into())],
            return_type: box None,
        };
        let (plat, mut graph, mut globals) = test_setting();
        // extern syscall_exit fun(i32) -> ()
        globals.insert("syscall_exit".into(),
                       DataType::Composite(CompositeType::Fun(intvoid.clone())));
        let mut locals = LocalScope::new(&globals);
        let r = flowgen_expr(&FunCall(box Ident("syscall_exit".into()), vec![Literal(bigint(1))]),
                             &plat,
                             &mut graph,
                             &mut locals);
        let newslots = r.expect("function flowgen should not fail");
        // make sure nodes are as expected
        assert_eq!(newslots.len(), 0);
        assert_eq!(graph.nodes.len(), 3);
        assert_eq!(graph.localslots.len(), 2);
        assert_eq!(graph.nodes[0].action, NodeAction::CopyOnly);
        assert_eq!(graph.nodes[1].action, NodeAction::CopyOnly);
        assert_eq!(graph.nodes[2].action, NodeAction::Call(intvoid));
        // slot for arg is created after slot for callee
        assert_eq!(graph.nodes[2].inputs, vec![1, 0]);
    }

    #[test]
    fn assign() {
        let (plat, mut graph, mut globals) = test_setting();
        // extern num i32
        globals.insert("num".into(), DataType::Basic("i32".into()));
        let mut locals = LocalScope::new(&globals);
        let newslots = flowgen_expr(&Assign(box Ident("num".into()), box Literal(bigint(17))),
                                    &plat,
                                    &mut graph,
                                    &mut locals)
                           .expect("assignment flowgen failed");
        assert_eq!(newslots.len(), 0);
        assert_eq!(graph.nodes.len(), 2);
        assert_eq!(graph.nodes[0].hwreqs.variables[graph.nodes[0].hwreqs.afters[0]],
                   HwLoc::Imm(bigint(17)).into());
        assert_eq!(graph.nodes[1].hwreqs.variables[graph.nodes[1].hwreqs.befores[0]],
                   HwLoc::labelled_var("num").into());
    }
}
