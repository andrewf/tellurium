use flowgraph::*;
use common::*;
use parsetree;

pub struct CheckedFunDef {
    pub ld_name: String,
    pub signature: FunSignature,
    pub body: FlowGraph,
}

pub struct CheckedProgram {
    pub externs: Vec<String>,
    pub function_definitions: Vec<CheckedFunDef>,
    pub global_vars: Vec<VarDef>,
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

pub type GlobalVarScope = HashMap<String, DataType>;

enum ScopeParent<'a> {
    Local(&'a LocalScope),
    Global(&'a GlobalVarScope),
}

struct LocalScope<'a> {
    parent: ScopeParent<'a>,
    current: HashMap<String, ActionInput>,
}

impl LocalScope<'a> {
    fn new(parent: ScopeParent<'a>) -> Self {
        LocalScope{
            parent: parent,
            current: HashMap::new(),
        }
    }
    fn get(&self, name: String) -> Option<ActionInput> {
        unimplemented!()
    }
    fn add(&mut self, name: String, value: ActionInput) {
        unimplemented!()
    }
}


// put all toplevel items in a single scope so we can look them
// up all together.
fn make_global_scopes<'a>(functions: &Vec<FunDef>,
                          vars: &Vec<VarDef>,
                          externs: &Vec<ExternDef>)
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


fn flowgen_file(toplevel: parsetree::TopLevel) -> Result<CheckedProgram, Error> {
    let (functions, globals, externs) = demux_toplevel(toplevel);
    let global_scope = make_global_scope(functions, globals, externs);
    let checked_funs : Vec<CheckedFunDef> = functions.into_iter().map(|function| {
        let graph = flowgen_function(function, global_scope);
        CheckedFunDef {
            ld_name: function.ld_name,
            signature: function.signature,
            body: graph,
        }
    }).collect::<Result<Vec<_>>>()?;
    CheckedProgram {
        externs: externs,
        function_definitions: checked_funs,
        global_vars: globals,
    }
}

fn flowgen_function(fundef: parsetree::FunDef, globals: GlobalVarScope) -> Result<Flowgraph, Error> {
    let mut graph = FlowGraph::new();
    // init state
    let init_state = ?;
    // init stmt
    let mut init_scope = LocalScope::new(globals);
    for argname in fundef.argnames.iter() {
        let mut argvar = Var::new();
        init_scope.add(argname, ActionInput::Var(graph.vars.new_from(argvar)))
    }
    // initial scope, vars
    let final_continuation = JumpTarget::Ret;
    
)

// return last statement.
fn flowgen_block(block: &parsetree::Block, scope: LocalScope, continuation: Continuation) -> Result<usize, Error> {
    //

}

// return last statement.
fn flowgen_expr(expr: &parsetree::Expression, scope: &LocalScope, continuation: Continuation) -> Result<usize, Error> {
    //
    use parsetree::Expression::*;
    match expr {
        &Ident(ref name) => {
            match scope.lookup(name) {
                Some(actioninput) => Ok(Some(actioninput)),
                None => Err(mkerr(format!("couldn't find name {}", name)))
            }
        }
        &FunCall(box ref callee, ref args) => {
            // create statement 
            // flowgen callee
            let callee_inp = flowgen_expr(callee, scope, 
        }
        &Literal(ref val) => {
        }
        &PtrDeref(ref box ptr) => {
        }
        &Address(ref box) => {
        }
        _ => Err(mkerr("unsupported expr type"))
    }
}

fn flowgen_condition(cond: &parsetree::Expression, yes: &parsetree::Block, else: Option<&parsetree::Block, scope: &LocalScope, continuation: Continuation) -> Result<usize, Error> {
    //
}

