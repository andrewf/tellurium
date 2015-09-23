use num::traits::ToPrimitive;
//use parsetree::*;
use parsetree::Expression;
use parsetree::Expression::*;
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

// current types, value scope, function scope
// useful in building a FlowGraph
struct NameState {
}

impl NameState {
    fn new() -> NameState {
        NameState{}
    }
    fn get_value(&self, name: &String) -> Result<(Location, DataType), NameError> {
    }
    fn get_type(&self, name: &String) -> Result<DataType, NameError> {
    }
    fn dispatch(&self, name: &String, sig: &FunSignature)
        -> Result<(Location, FunSignature), DispatchError> {
    }
    fn push_frame(&mut self) {}
    fn pop_frame(&mut self) {}
    fn set_name(&mut self, name: &String, slot: uint){
    }
}

//
fn parseglobals<'a>(tl: &'a parsetree::TopLevel, platform: &Platform)
    -> Result<(HashMap<String, &'a FunDef>,
               HashMap<String, &'a VarDef>,
               GlobalTypeNamespace), Error>
{
    // high-level survey of the code, just enough to enable calling the functions
    // and accessing the vars in later codegen passes
    // in C terms, just the prototypes.
    // types first, so we can get the real type objects in the fundefs
    let mut types = platform.get_basic_types();
    // now vars and fns
    let mut globals = HashMap::new();
    let mut functions = HashMap::new();
    // get out vars, functions
    for item in tl.iter() {
        match item {
            &TopLevelItem::VarDef(ref v) => {
                let dt = make_data_type(types, v.datatype);
                // ignore initial value for now?
                globals.insert(v.ld_name, GlobalVar {
                    ld_name: v.ld_name,
                    datatype: dt
                })
            }
            &TopLevelItem::FunDef(ref f) => {
                let fullname = f.ld_name;
                functions.insert(fullname, f.clone())

            }
        }
    }
    Ok((functions, globals, types))
}

// take a parsed function and generate statement graph. 
fn dothething(globalfuns: Scope<typed::FunDef>, globalvals: Scope<typed::VarDef>)
    -> Result<Block, Error>
{
}

fn flowgen_fun(globals, fundef: &FunDef) -> Result<FlowGraph, Error> {
    let mut graph = FlowGraph{ stmts: Vec::new(), localslots: Vec::new() };
    let mut namestate = NameState::new();
    namestate.push_frame();
    try!(flowgen_block(&mut namestate, &mut graph, fundef.body));
    return graph
}

fn flowgen_block(names: &mut NameState,
                 graph: &mut FlowGraph,
                 syntax: &Block) -> Result<(), Error>
{
    for stmt in syntax.iter() {
        match stmt {
            Statement::Expr(e) => {
                // we're ignoring return value
                // someday we'll do a warning
                try!(flowgen_expr(names, graph, e));
            }
            Statement::Var(VarDef{ld_name, datatype, init}) => {
                // process init expression
                let initslots = try!(flowgen_expr(names, graph, e));
                match initslots {
                    // only support single-width expressions for now
                    Single(initslot) => {
                        // make slot, assign
                        let slot = graph.new_slot(datatype);
                        names.set_name(ld_name, slot);
                    },
                    _=>{unimplemented!()}
                }

            }
            Statement::Return(e) => {
                // depends on protocol
                // jr $ra or whatever
            }
            Statement::Condition(_,_,_) => {
                unimplemented!()
            }
        }
    }
}

fn flowgen_expr(names: &mut NameState, graph: &mut FlowGraph, e: &Expression) -> Result<uint, Error> {
    match e {
        FunCall(box Ident(ref name), args) => {
            // emit funcall
            let (loc, sig) = try!(names.dispatch(name));
            let ret = graph.new_slot(def.return_type);
            // have to resolve args to specific slots
            let args_as_slots = Vec::<uint>new();
            for arg in args.iter() {
                let slot = try!(flowgen_expr(names, graph, arg));
                args_as_slots.push(slot)
            }
            // type check. yeah, that's what this is.
            if args_as_slots.len() != sig.argtypes.len() {
                return Err(DispatchError::WrongNumberOfArgs));
            }
            for (arg_slot, param_type) in args_as_slots.iter().zip(sig.argtypes) {
                if graph.localslots[arg_slot] != param_type {
                    return Err(DispatchError::WrongArgType)
                }
            }
            // make funcall statement
            graph.stmts.push(Statement{
                action: StmtAction::Call(loc, sig),
                inputs: args_as_slots,
                outputs: returns
            });
            Ok(returns)
        }
        Ident(ref s) => {
            // look up in scope
            try!(names.get_value(s))
        }
        Literal(ref val) => {
            let slot = graph.new_slot(types.get("i32"));
            graph.stmts.push(Statement{
                action: StmtAction::Imm(val.clone()),
                inputs: Vec::new(),
                outputs: slot
            })
        }
        Assign(box Ident(ref name), box rvalue) => {
            // create variable assignment, meaning...
            unimplemented!()
        }
        _ => {
            unimplemented!()
        }
    }
}


fn flowgen<W: Write>(out: &mut W, tl: &parsetree::TopLevel) {
    writeln!("flowgen soon to come")
}
