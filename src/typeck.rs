use num::traits::ToPrimitive;
//use parsetree::*;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use parsetree;
use parsetree::Expression;
use parsetree::Expression::*;
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

// typecheck the program in tl and return it in a graph-based
// format suitable for codegen
pub fn check_and_flowgen(tl: parsetree::TopLevel, platform: Platform)
    -> Result<CheckedProgram, Error>
{
    let mut types: GlobalTypeNamespace = platform.get_basic_types();
    let (functions, vars, externs) = demux_toplevel(tl);
    // ignore vars for now
    // block here to constrain lifetime of borrows of functions, externs
    // flow_graphs is a parallel array to functions
    // gen seperately so we can borrow functions while making
    let flow_graphs = {
        let function_scope = try!(make_function_scope(&functions, &externs));
        // use function_scope to build the graph for each function
        let mut zzz = Vec::new();
        // generate body flow graphs
        for f in functions.iter() {
            zzz.push(try!(flowgen_function(&function_scope, &f)))
        }
        zzz
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
        global_vars: Vec::new()
    })
}

fn make_function_scope<'a>(functions: &'a Vec<FunDef>, externs: &'a Vec<ExternDef>)
    -> Result<FunctionScope<'a>, Error>
{
    let mut function_scope : HashMap<String, GlobalFunction> = HashMap::new();
    // build function scope
    // first put in extern declarations
    for ext in externs.iter() {
        if let &DataType::Composite(CompositeType::Fun(ref sig)) = &ext.datatype {
            match function_scope.entry(ext.ld_name.clone()) {
                Entry::Vacant(vac) => {
                    vac.insert(GlobalFunction {
                        sig: sig,
                        location: GlobalFunctionLocation::Extern
                    });
                }
                Entry::Occupied(_) => {
                    return Err(mkerr("duplicate extern".to_string()));
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
                        return Err(mkerr("conflicting extern and function declarations".to_string()));
                    }
                    GlobalFunctionLocation::Declared => {
                        return Err(mkerr("conflicting function declarations".to_string()));
                    }
                }
            }
        }
    }
    Ok(function_scope)
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

// take a parsed function and generate statement graph. 
//fn dothething(globalfuns: Scope<typed::FunDef>, globalvals: Scope<typed::VarDef>)
//    -> Result<Block, Error>
//{
//}

fn flowgen_function<'a>(function_scope: &'a FunctionScope<'a>, fundef: &FunDef) -> Result<FlowGraph, Error> {
    let mut graph = FlowGraph::new();
    graph.stmts.push(Statement {
        action: StmtAction::Return,
        inputs: Vec::new(),
        outputs: Vec::new()
    });
    //let mut namestate = NameState::new();
    //namestate.push_frame();
    //try!(flowgen_block(&mut namestate, &mut graph, fundef.body));
    Ok(graph)
}

//fn flowgen_block(names: &mut NameState,
//                 graph: &mut FlowGraph,
//                 syntax: &Block) -> Result<(), Error>
//{
//    for stmt in syntax.iter() {
//        match stmt {
//            Statement::Expr(e) => {
//                // we're ignoring return value
//                // someday we'll do a warning
//                try!(flowgen_expr(names, graph, e));
//            }
//            Statement::Var(VarDef{ld_name, datatype, init}) => {
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
//            Statement::Return(e) => {
//                // depends on protocol
//                // jr $ra or whatever
//            }
//            Statement::Condition(_,_,_) => {
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
//            graph.stmts.push(Statement{
//                action: StmtAction::Call(loc, sig),
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
//            graph.stmts.push(Statement{
//                action: StmtAction::Imm(val.clone()),
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


