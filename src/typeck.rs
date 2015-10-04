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

type FunctionScope<'a> = HashMap<String, GlobalFunction<'a>>;
type VarScope = HashMap<String, DataType>;

// typecheck the program in tl and return it in a graph-based
// format suitable for codegen
pub fn check_and_flowgen(tl: parsetree::TopLevel, platform: Platform)
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
    -> Result<(FunctionScope<'a>, VarScope), Error>
{
    let mut function_scope : HashMap<String, GlobalFunction> = HashMap::new();
    let mut var_scope = VarScope::new();
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

// current types, value scope, function scope
// useful in building a FlowGraph
//struct NameState {
//}
//
//impl NameState {
//    fn new() -> NameState {
//        NameState{}
//    }
//    fn get_value(&self, name: &String) -> Result<(Location, DataType), NameError> {
//    }
//    fn get_type(&self, name: &String) -> Result<DataType, NameError> {
//    }
//    fn dispatch(&self, name: &String, sig: &FunSignature)
//        -> Result<(Location, FunSignature), DispatchError> {
//    }
//    fn push_frame(&mut self) {}
//    fn pop_frame(&mut self) {}
//    fn set_name(&mut self, name: &String, slot: uint){
//    }
//}

fn flowgen_function<'a>(function_scope: &'a FunctionScope<'a>,
                        var_scope: &'a VarScope,
                        fundef: &FunDef)
    -> Result<FlowGraph, Error>
{
    let mut graph = FlowGraph::new();
    // go through body and add statements
    for s in fundef.body.iter() {
        match s {
            &Statement::Expr(FunCall(box Ident(ref name), ref args)) => {
                // normal function call
                // look up function
                match function_scope.get(name) {
                    Some(ref global_fun) => {
                        graph.stmts.push(Node {
                            action: NodeAction::Call(Location::Labeled((*name).clone()),
                                                     global_fun.sig.clone()),
                            // forget args
                            inputs: Vec::new(),
                            outputs: Vec::new()
                        });
                    }
                    None => {
                        return mkerr("unknown function name")
                    }
                }
            }
            &Statement::Expr(Assign(box Ident(ref assignee), box ref val)) => {
                match val {
                    &Ident(ref name) => {
                        graph.stmts.push(Node {
                            action: NodeAction::Assign(assignee.clone()),
                            inputs: vec![Location::Labeled(name.clone())],
                            outputs: vec![]
                        });
                    }
                    _ => {
                        return mkerr("unsupported assignment statement");
                    }
                }
            }
            _ => {
                unimplemented!()
            }
        }
    }
    //try!(flowgen_block(&mut namestate, &mut graph, fundef.body));
    Ok(graph)
}

//fn flowgen_block(names: &mut NameState,
//                 graph: &mut FlowGraph,
//                 syntax: &Block) -> Result<(), Error>
//{
//    for stmt in syntax.iter() {
//        match stmt {
//            Node::Expr(e) => {
//                // we're ignoring return value
//                // someday we'll do a warning
//                try!(flowgen_expr(names, graph, e));
//            }
//            Node::Var(VarDef{ld_name, datatype, init}) => {
//                // process init expression
//                let initslots = try!(flowgen_expr(names, graph, e));
//                match initslots {
//                    // only support single-width expressions for now
//                    Single(initslot) => {
//                        // make slot, assign
//                        let slot = graph.new_slot(datatype);
//                        names.set_name(ld_name, slot);
//                    },
//                    _=>{unimplemented!()}
//                }
//
//            }
//            Node::Return(e) => {
//                // depends on protocol
//                // jr $ra or whatever
//            }
//            Node::Condition(_,_,_) => {
//                unimplemented!()
//            }
//        }
//    }
//}
//
//fn flowgen_expr(names: &mut NameState, graph: &mut FlowGraph, e: &Expression) -> Result<uint, Error> {
//    match e {
//        FunCall(box Ident(ref name), args) => {
//            // emit funcall
//            let (loc, sig) = try!(names.dispatch(name));
//            let ret = graph.new_slot(def.return_type);
//            // have to resolve args to specific slots
//            let args_as_slots = Vec::<uint>new();
//            for arg in args.iter() {
//                let slot = try!(flowgen_expr(names, graph, arg));
//                args_as_slots.push(slot)
//            }
//            // type check. yeah, that's what this is.
//            if args_as_slots.len() != sig.argtypes.len() {
//                return Err(DispatchError::WrongNumberOfArgs);
//            }
//            for (arg_slot, param_type) in args_as_slots.iter().zip(sig.argtypes) {
//                if graph.localslots[arg_slot] != param_type {
//                    return Err(DispatchError::WrongArgType)
//                }
//            }
//            // make funcall statement
//            graph.stmts.push(Node{
//                action: NodeAction::Call(loc, sig),
//                inputs: args_as_slots,
//                outputs: returns
//            });
//            Ok(returns)
//        }
//        //Ident(ref s) => {
//        //    // look up in scope
//        //    try!(names.get_value(s))
//        //}
//        Literal(ref val) => {
//            let slot = graph.new_slot(types.get("i32"));
//            graph.stmts.push(Node{
//                action: NodeAction::Imm(val.clone()),
//                inputs: Vec::new(),
//                outputs: slot
//            })
//        }
//        //Assign(box Ident(ref name), box rvalue) => {
//        //    // create variable assignment, meaning...
//        //    unimplemented!()
//        //}
//        _ => {
//            unimplemented!()
//        }
//    }
//}


