// interface types between parsetree, typeck, probably others

use std::collections::HashMap;
use tuplity::Tuplity;

pub struct Error {
    pub msg: String,
}

pub fn mkerr(s: String) -> Error {
    Error {
        msg: s
    }
}


// A type that can be assigned a size, can be addressed.
// P is the type of non-composite types on target machine
#[derive(Debug,Clone)]
pub enum CompositeType {
    Pointer(Box<DataType>),
    Array(u64, Box<DataType>),
    Struct(Vec<(String, DataType)>), // TODO packing protocol
    Fun(FunSignature)
}

// a named type or composition thereof
// 'Basic' need not be truly atomic, may be a reference
// to another composite type in global namespace
#[derive(Debug,Clone)]
pub enum DataType {
    Basic(String),
    Composite(CompositeType)
}

pub type VarType = Tuplity<DataType>;

// pieces necessary to call the function, specifically to generate
// the call-site prelude and suffix, once we have
// the actual address of it.
#[derive(Debug,Clone)]
pub struct FunSignature {
    pub argtypes: Vec<VarType>,
    pub return_type: Box<Option<VarType>>
    //pub convention: String
}

impl FunSignature {
    fn new(a: Vec<VarType>, r: Option<VarType>) -> FunSignature {
        FunSignature {
            argtypes: a,
            return_type: box r
        }
    }
}

// if a type is None, that means it's a primitive
pub type GlobalTypeNamespace = HashMap<String, (u64, Option<DataType>)>;

// stuff everyone needs to know about the native platform
// well, not so much the parser, but the type checker.
pub struct Platform;

impl Platform {
    // these are types that the hardware knows how to operate on
    // names are part of the protocol between platform and typechecker
    pub fn get_basic_types(&self) -> GlobalTypeNamespace {
        let mut result = HashMap::with_capacity(10);
        result.insert("u32".to_string(), (4, None));
        result.insert("i32".to_string(), (4, None));
        result.insert("u8".to_string(), (1, None));
        result.insert("ptr_t".to_string(), (4, None));
        return result
    }
    pub fn get_pointer_type(&self, pointee: &DataType) -> Result<String, Error> {
        Ok("ptr_t".to_string())
    }
}

