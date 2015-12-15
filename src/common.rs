// interface types between parsetree, typeck, probably others

use std::collections::HashMap;

use num::BigInt;

#[derive(Debug,Clone)]
pub struct Error {
    pub msg: String,
}

pub fn mkerr<S: ToString + ?Sized, T>(s: &S) -> Result<T, Error> {
    Err(Error {
        msg: s.to_string()
    })
}


// A type that can be assigned a size, can be addressed.
// P is the type of non-composite types on target machine
#[derive(Debug,Clone,PartialEq)]
pub enum CompositeType {
    Pointer(Box<DataType>),
    Array(u64, Box<DataType>),
    //Struct(Vec<(String, DataType)>), // TODO packing protocol
    Fun(FunSignature)
}

// a named type or composition thereof
// 'Basic' need not be truly atomic, may be a reference
// to another composite type in global namespace
#[derive(Debug,Clone,PartialEq)]
pub enum DataType {
    Basic(String),
    Composite(CompositeType)
}

// place you can put a local variable
#[derive(Debug, Clone,PartialEq)]
pub enum HwLoc {
    Register(String), // name without any % or $
    //Stack(i64, u64), // offset from stack pointer, whatever that may be, and size
    Label(String),
    Imm(BigInt),
}

// Set of possibilities for the hardware location of an input or
// output to a stmt
// For now, either a specific location, or no restriction
#[derive(Debug,Clone,PartialEq)]
pub struct HwRange(Option<HwLoc>); // so hack. much gross.

impl HwRange {
    pub fn new() -> HwRange {
        HwRange(None)
    }
}

impl From<HwLoc> for HwRange {
    fn from(h: HwLoc) -> HwRange {
        HwRange(Some(h))
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct HwReqs {
    pub befores: Vec<HwRange>,
    pub afters: Vec<HwRange>
}

impl HwReqs {
    pub fn new() -> HwReqs {
        HwReqs {
            befores: Vec::new(),
            afters: Vec::new(),
        }
    }
}

// pieces necessary to call the function, specifically to generate
// the call-site prelude and suffix, once we have
// the actual address of it.
#[derive(Debug,Clone,PartialEq)]
pub struct FunSignature {
    pub argtypes: Vec<DataType>,
    pub return_type: Box<Option<DataType>>
    //pub convention: String
}

// if a type is None, that means it's a primitive
pub type GlobalTypeNamespace = HashMap<String, (u64, Option<DataType>)>;

