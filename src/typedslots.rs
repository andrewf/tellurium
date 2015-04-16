struct TypeError {
    msg: String
    // add context
}

fn mktypeerr(s: String) {
    TypeError {
        msg: s
    }
}

// independently storable data (no tuples). tuples are treated
// as a sequence of DataTypes (since a lot of the point is that
// the elements aren't actually tied together that tightly
// main point of this is to associate sizes with all the types.
// sizes are u64, so platform of compiler doesn't limit sizes
// in compiled code
enum DataType {
    Void,
    SignedInt(u64),
    UnsignedInt(u64),
    Array(u64, Box<DataType>),
    Pointer(Box<DataType>)
}

// convert parsetree::DataType to real datatype
fn make_data_type(typescope: &Scope<String, DataType>, input: &parsetree::DataType)
    -> Result<DataType, WhoKnows>
{
    match input {
        &Void => Ok(DataType::Void),
        &Pointer(ref box referrent) => {
            let r = try!(make_data_type(referrent));
            Ok(DataType::Pointer(r))
        },
        &Array(parsetree::Expression::Literal(ref size), elemtype) => {
            // crap, have to evaluate the size expression?
        }
        &Named(ref name) => {
            match typescope.get(name) {
                Some(existing) => {
                    Ok(existing)
                }
                None => {
                    Err(i dont even...)
                }
            }
        }
        _ => {
            Err(cant even)
        }
    }
}


// well, prototype. enough to emit calls
struct FunDef {
    ld_name: String,
    //convention: String,
    return_type: Vec<DataType>,
    args: Vec<DataType>
}

struct GlobalVar {
    ld_name: String,
    datatype: DataType
}


enum Port {
    Global(GlobalVar),
    Local(usize) // index to Block::localslots
}

enum StmtAction {
    Call(&FunDef), // args, return in container struct
    Assign,  // to a mem-var
    Return
    // ???
    // assembly
    // condition, loop will contain Vec<Statement>
}

// represents a single executable statement with
// "slot" inputs and outputs. Basically a function call
// with args already evaluated, or other primitive statement
struct Statement {
    // some sort of content, I guess. looked-up fn, etc
    // something you can directly turn into an assembly snippet
    action: StmtAction,
    inputs: Vec<Port>,
    outputs: Vec<Port>  // real type, not parsetree::DataType
}

struct Scope<K, V> {
    parent: Option<&Scope<K, V>>,
    items: HashMap<K, V>
}

impl<K, V> Scope<K, V> {
    fn lookup(&self, key: &str) -> Option<V> {
        // try in items
        // if parent, try that
        // otherwise, none
    }
}

struct FullFunName {
    name: String,
    args: Vec<DataType>
}

struct LocalVar

struct Block {
    // these must be typechecked!
    stmts: Vec<Statement>, // idea is that for codegen, just go through one at a time
    localslots: Vec<DataType>
}

//
fn parseglobals(tl: &parsetree::TopLevel)
    -> Result<(Scope<FullFunName, typed::FunDef>,
               Scope<String,      typed::GlobalVar>,
               Scope<String,      typed::DataType>), WhoKnows>
{
    // high-level survey of the code, just enough to enable calling the functions
    // and accessing the vars in later codegen passes
    // in C terms, just the prototypes.
    // types first, so we can get the real type objects in the fundefs
    let mut types = HashMap::with_capacity(8);
    types.insert("i8".to_string(), DataType::SignedInt(1));
    types.insert("i16".to_string(), DataType::SignedInt(2));
    types.insert("i32".to_string(), DataType::SignedInt(4));
    types.insert("i64".to_string(), DataType::SignedInt(8));
    types.insert("u8".to_string(), DataType::UnsignedInt(1));
    types.insert("u16".to_string(), DataType::UnsignedInt(2));
    types.insert("u32".to_string(), DataType::UnsignedInt(4));
    types.insert("u64".to_string(), DataType::UnsignedInt(8));
    let types = Scope{ parent: None, types };
    // now vars and fns
    let mut globals = HashMap::new();
    let mut functions = HashMap::new();
    for item in tl.iter() {
        match item {
            &TopLevelItem::VarDef(ref v) => {
                let dt = make_data_type(v.datatype);
                // ignore initial value for now?
                globals.insert(v.ld_name, GlobalVar {
                    ld_name: v.ld_name,
                    datatype: dt
                }
            }
            &TopLevelItem::FunDef(ref f) => {
            }
        }
    }
}

// take a parsed function and generate statement graph. 
fn dothething(globalfuns: Scope<typed::FunDef>, globalvals: Scope<typed::VarDef>)
    -> Result<Block, TypeError>
{
}
