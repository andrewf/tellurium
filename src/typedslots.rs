use num::traits::ToPrimitive;
//use parsetree::*;

struct TypeError {
    msg: String,
}

fn mktypeerr(s: String) {
    TypeError {
        msg: s
    }
}

// argument for DataType
enum PrimitiveType {
    SignedInt(u64),
    UnsignedInt(u64)
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
    inputs: Vec<uint>,
    outputs: Vec<uint>  // real type, not parsetree::DataType
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

struct Block {
    // these must be typechecked!
    stmts: Vec<Statement>, // idea is that for codegen, just go through one at a time
    localslots: Vec<DataType> // temporary values, inputs and outputs for statements
}

//
fn parseglobals(tl: &parsetree::TopLevel)
    -> Result<(Scope<String, FunDef>,
               Scope<String, Vardef>,
               Scope<String, DataType>), WhoKnows>
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
    // get out vars, functions
    for item in tl.iter() {
        match item {
            &TopLevelItem::VarDef(ref v) => {
                let dt = make_data_type(types, v.datatype);
                // ignore initial value for now?
                globals.insert(v.ld_name, GlobalVar {
                    ld_name: v.ld_name,
                    datatype: dt
                }
            }
            &TopLevelItem::FunDef(ref f) => {
                let fullname = 
                functions.insert(

            }
        }
    }
}

// take a parsed function and generate statement graph. 
fn dothething(globalfuns: Scope<typed::FunDef>, globalvals: Scope<typed::VarDef>)
    -> Result<Block, TypeError>
{
}
