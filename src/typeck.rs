use num::traits::ToPrimitive;
//use parsetree::*;
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

//struct Scope<K, V> {
//    parent: Option<&Scope<K, V>>,
//    items: HashMap<K, V>
//}
//
//impl<K, V> Scope<K, V> {
//    fn lookup(&self, key: &str) -> Option<V> {
//        // try in items
//        // if parent, try that
//        // otherwise, none
//    }
//}

// we'll need to know, for error messaging if nothing else,
// where a function came from in the struct below
enum GlobalFunctionLocation {
    Extern,
    Declared
}

// callable function in global scope, whether extern or 
// defined in Te code
struct GlobalFunction<'a> {
    sig: &'a FunSignature,
    location: GlobalFunctionLocation
}

pub type FunctionScope<'a> = HashMap<String, GlobalFunction<'a>>;
pub type GlobalVarScope = HashMap<String, DataType>;

pub struct LocalScope<'a> {
    globals: &'a GlobalVarScope,
    locals: HashMap<String, usize>, // string -> slot
}

impl<'a> LocalScope<'a> {
    pub fn new(glob: &'a GlobalVarScope) -> Self {
        LocalScope {
            globals: glob,
            locals: HashMap::new()
        }
    }
    pub fn put(&mut self, name: &String, slot: usize) {
        self.locals.insert(name.clone(), slot);
    }
    pub fn get(&self, name: &String, graph: &mut FlowGraph)
        -> Result<(usize, DataType), Error>
    {
        match self.locals.get(name) {
            Some(slot) => {
                Ok((*slot, graph.localslots[*slot].clone()))
            }
            None => {
                match self.globals.get(name) {
                    Some(ref dt) => {
                        let slot = graph.new_slot(dt);
                        let n = node_from_hw(HwLoc::Label(name.clone()), slot);
                        graph.nodes.push(n);
                        Ok((slot, (*dt).clone()))
                    }
                    None => mkerr(&format!("couldn't find var {}", name))
                }
            }
        }
    }
}

// split all the toplevel items into separate vectors
fn demux_toplevel(tl: parsetree::TopLevel)
    -> (Vec<FunDef>, Vec<VarDef>, Vec<ExternDef>)
{
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
            TopLevelItem::FunDef(f) => {
                functions.push(f)
            }
            TopLevelItem::ExternDef(e) => {
                externs.push(e)
            }
        }
    }
    (functions, globals, externs)
}

// typecheck the program in tl and return it in a graph-based
// format suitable for codegen
pub fn check_and_flowgen<P: Platform>(tl: parsetree::TopLevel, platform: &P)
    -> Result<CheckedProgram, Error>
{
    let mut types: GlobalTypeNamespace = platform.get_basic_types();
    let (functions, vars, externs) = demux_toplevel(tl);
    // ignore vars for now
    // flow_graphs is a parallel array to functions
    // generate seperately so we can borrow functions while making it
    let flow_graphs = {
        let (function_scope, var_scope) = try!(make_global_scopes(&functions, &vars, &externs));
        // use function_scope to build the graph for each function
        let mut flow_graphs = Vec::new();
        // generate body flow graphs
        for f in functions.iter() {
            flow_graphs.push(try!(flowgen_function(&function_scope, &var_scope, &f)))
        }
        flow_graphs
    };

    let checked_functions = functions.into_iter().zip(flow_graphs).map(|(f, b)| {
        CheckedFunDef {
            ld_name: f.ld_name,
            signature: f.signature,
            body: b
        }
    }).collect();

    return Ok(CheckedProgram{
        externs: externs.into_iter().map(|e| e.ld_name).collect(),
        function_definitions: checked_functions,
        global_vars: vars
    })
}

// parses a list of FunDefs and ExternDefs into a hashmap
// so we can look up functions by name without a linear search
// through two data structures.
// also make global variable scope
// also ensure there are no duplicate names
fn make_global_scopes<'a>(functions: &'a Vec<FunDef>,
                          vars: &'a Vec<VarDef>,
                          externs: &'a Vec<ExternDef>)
    -> Result<(FunctionScope<'a>, GlobalVarScope), Error>
{
    let mut function_scope : HashMap<String, GlobalFunction> = HashMap::new();
    let mut var_scope = GlobalVarScope::new();
    // build function scope
    // first put in extern declarations
    for ext in externs.iter() {
        match &ext.datatype {
            // if it's an external function, add it to function scope
            &DataType::Composite(CompositeType::Fun(ref sig)) => {
                match function_scope.entry(ext.ld_name.clone()) {
                    Entry::Vacant(vac) => {
                        vac.insert(GlobalFunction {
                            sig: sig,
                            location: GlobalFunctionLocation::Extern
                        });
                    }
                    Entry::Occupied(_) => {
                        return mkerr("duplicate extern");
                    }
                }
            }
            // otherwise, it's just a var
            ref t => {
                match var_scope.entry(ext.ld_name.clone()) {
                    Entry::Vacant(vac) => {
                        vac.insert((*t).clone());
                    }
                    _ => {
                        return mkerr("duplicate var");
                    }
                }
            }
        }
    }
    // put in Te-defined functions
    for fun in functions.iter() {
        match function_scope.entry(fun.ld_name.clone()) {
            Entry::Vacant(vac) => {
                vac.insert(GlobalFunction {
                    sig: &fun.signature,
                    location: GlobalFunctionLocation::Declared
                });
            }
            Entry::Occupied(occ) => {
                match occ.get().location {
                    GlobalFunctionLocation::Extern => {
                        return mkerr("conflicting extern and function declarations");
                    }
                    GlobalFunctionLocation::Declared => {
                        return mkerr("conflicting function declarations");
                    }
                }
            }
        }
    }
    // Te-defined vars
    for v in vars.iter() {
        match var_scope.entry(v.ld_name.clone()) {
            Entry::Vacant(vac) => {
                vac.insert(v.datatype.clone());
            }
            _ => {
                return mkerr(&format!("duplicate var {}", v.ld_name))
            }
        }
    }
    Ok((function_scope, var_scope))
}

// current types, value scope, function scope
// useful in building a FlowGraph
//struct NameState<'a> {
//    graph: &'a FlowGraph,
//    global_functions: &'a FunctionScope<'a>,
//    global_vars: &'a GlobalVarScope,
//    local_vars: HashMap<String, (usize, &'a DataType)>
//}
//
//impl<'a> NameState<'a> {
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
////    fn dispatch(&self, name: &String, sig: &FunSignature)
////        -> Result<(Location, FunSignature), DispatchError> {
////    }
////    fn push_frame(&mut self) {}
////    fn pop_frame(&mut self) {}
//}

fn flowgen_function<'a>(function_scope: &'a FunctionScope<'a>,
                        var_scope: &'a GlobalVarScope,
                        fundef: &FunDef)
    -> Result<FlowGraph, Error>
{
    let mut graph = FlowGraph::new();
    let mut localscope = LocalScope::new(var_scope);
    // add slots for function args
    for (t, name) in fundef.signature.argtypes.iter().zip(fundef.argnames.iter()) {
        // TODO add to scope
        let index = graph.new_slot(t);
        graph.reqs.befores.push( HwRange::new() ); // todo calling convention
        localscope.put(name, index);
    }
    // go through body and add statements
    for s in fundef.body.iter() {
        match s {
            &Statement::Expr(ref expr) => {
                try!(flowgen_expr(expr, &mut graph, function_scope, &localscope));
            }
            _ => {
                unimplemented!()
            }
        }
    }
    //try!(flowgen_block(&mut namestate, &mut graph, fundef.body));
    Ok(graph)
}

pub fn flowgen_expr(expr: &Expression,
                graph: &mut FlowGraph,
                function_scope: &FunctionScope,
                locals: &LocalScope)
    -> Result<Vec<usize>, Error>
{
    match expr {
        &Literal(ref n) => {
            let slot = graph.new_slot(&DataType::Basic("i32".into()));
            let n = node_from_hw(HwLoc::Imm(n.clone()), slot);
            graph.nodes.push(n);
            Ok(vec![slot])
        }

        //&Ident(ref s) => {
        //    let (input, dt) = try!(locals.get(s, graph));
        //    Ok(vec![input])
        //}
        //&FunCall(box Ident(ref name), ref args) => {
        //    // normal function call
        //    // look up function
        //    match function_scope.get(name) {
        //        Some(ref global_fun) => {
        //            // just 0 or 1, for now
        //            let output_slots = match global_fun.sig.return_type {
        //                box Some(ref ret) => vec![graph.new_slot(ret)],
        //                box None => Vec::new()
        //            };
        //            let input_slots: Vec<NodeInput> = {
        //                let input_results =
        //                    args.iter()
        //                        .map(|e| -> Result<Edge, Error> {
        //                            flowgen_expr(e, graph, function_scope, locals)
        //                                .and_then(|v| v.into_iter().nth(0).ok_or(Error{msg:"arg is ()".into()}))
        //                        });
        //                try!(input_results.collect())
        //            }; // borrow graph
        //            // need version of output slots as NodeOutput's
        //            let wrapped_outputs = output_slots.iter()
        //                                          .map(|s| NodeOutput::from_slot(*s))
        //                                          .collect();
        //            graph.stmts.push(Node {
        //                action: NodeAction::Call(NodeInput::from_label((*name).clone()),
        //                                         global_fun.sig.clone()),
        //                inputs: input_slots,
        //                outputs: output_slots
        //            });
        //            Ok(wrapped_outputs)
        //        }
        //        None => {
        //            return mkerr("unknown function name")
        //        }
        //    }
        //}
        //&Assign(box Ident(ref assignee), box ref val) => {
        //    let (nodeinput, _t) = try!(locals.get(assignee, graph));
        //    if let Some(HwLoc::Label(_)) = nodeinput.hw {
        //        // assignee is global, no issue
        //    } else {
        //        return mkerr(&format!("can't assign to local {}", assignee))
        //    }
        //    let topush = {
        //        Node {
        //            action: NodeAction::Assign(assignee.clone()),
        //            inputs: vec![try!(flowgen_expr(val, graph, function_scope, locals)
        //                                .and_then(|v| v.into_iter().nth(0).ok_or(Error{msg:"assignee is ()".into()})))],
        //            outputs: vec![]
        //        }
        //    }; // borrow graph
        //    graph.stmts.push(topush);
        //    Ok(Vec::new())
        //}
        _ => { unimplemented!() }
    } // match
}

#[cfg(test)]
mod testy {
    use num::BigInt;
    use num::FromPrimitive;
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

    fn test_setting<'a>() -> (IntelPlatform, FlowGraph, FunctionScope<'a>, GlobalVarScope) {
        (IntelPlatform, FlowGraph::new(), HashMap::new(), GlobalVarScope::new())
    }

    #[test]
    fn zzz() {
        let (_, mut g, fscope, globals) = test_setting();
        let mut locals = LocalScope::new(&globals);
        let r = flowgen_expr(&Expression::Literal(FromPrimitive::from_i64(42).unwrap()),
                             &mut g,
                             &fscope,
                             &locals);
        let newslots = r.expect("flowgen of a literal should not fail. that is dumb");
        let expected_slot = 0;
        assert!(newslots[0] == expected_slot);
        assert!(g.localslots[expected_slot] == DataType::Basic("i32".into()));
    }
}

