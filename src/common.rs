// interface types between parsetree, typeck, probably others

use std::collections::HashMap;

use num::BigInt;
use num::traits::FromPrimitive;

#[derive(Debug,Clone)]
pub struct Error {
    pub msg: String,
}

pub fn mkerr<S: ToString + ?Sized, T>(s: &S) -> Result<T, Error> {
    Err(Error { msg: s.to_string() })
}

// A type that can be assigned a size, can be addressed.
// P is the type of non-composite types on target machine
#[derive(Debug,Clone,PartialEq)]
pub enum CompositeType {
    Pointer(Box<DataType>),
    Array(u64, Box<DataType>),
    // Struct(Vec<(String, DataType)>), // TODO packing protocol
    Fun(FunSignature),
}

// a named type or composition thereof
// 'Basic' need not be truly atomic, may be a reference
// to another composite type in global namespace
#[derive(Debug,Clone,PartialEq)]
pub enum DataType {
    Basic(String),
    Composite(CompositeType),
}
// pieces necessary to call the function, specifically to generate
// the call-site prelude and suffix, once we have
// the actual address of it.
#[derive(Debug,Clone,PartialEq)]
pub struct FunSignature {
    pub argtypes: Vec<DataType>,
    pub return_type: Box<Option<DataType>>,
    pub convention: Option<String>,
}

// if a type is None, that means it's a primitive
pub type GlobalTypeNamespace = HashMap<String, (u64, Option<DataType>)>;

pub fn bigint(n: i64) -> BigInt {
    FromPrimitive::from_i64(n).unwrap()
}
