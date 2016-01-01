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

// place you can put a local variable
#[derive(Debug,Clone,PartialEq)]
pub enum HwLoc {
    Register(String), // name without any % or $
    Label(String), // essentially an address literal. wrap in Mem to get value here
    Imm(BigInt), // immediate/constant value
    // The address of the top of the stack
    Stack, // generally used with Mem
    Mem(Box<HwLoc>, i64), // location to deref, offset
}

impl HwLoc {
    pub fn labelled_var<S: ToString + ?Sized>(s: &S) -> HwLoc {
        HwLoc::Mem(box HwLoc::Label(s.to_string()), 0)
    }
    pub fn from_regname<S: ToString + ?Sized>(s: &S) -> HwLoc {
        HwLoc::Register(s.to_string())
    }
    pub fn stack(offset: i64) -> HwLoc {
        HwLoc::Mem(box HwLoc::Stack, offset)
    }
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
    // if the range's only possible value is a single hwloc, return that
    // else return None
    pub fn concrete(&self) -> Option<&HwLoc> {
        if let &HwRange(Some(ref loc)) = self {
            Some(loc)
        } else {
            None
        }
    }
}

impl From<HwLoc> for HwRange {
    fn from(h: HwLoc) -> HwRange {
        HwRange(Some(h))
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct HwReqs {
    // store one master list of hardware variables,
    // into which the other members store indices
    // this enables the expression of requirements that,
    // e.g. an input be the same as an output.
    pub variables: Vec<HwRange>,
    pub befores: Vec<usize>,
    pub afters: Vec<usize>,
    pub clobbers: Vec<usize>, // TODO ensure freedom from conflict within variables
}

impl HwReqs {
    pub fn new() -> HwReqs {
        HwReqs {
            variables: Vec::new(),
            befores: Vec::new(),
            afters: Vec::new(),
            clobbers: Vec::new(),
        }
    }
    pub fn with_befores(b: Vec<HwRange>) -> HwReqs {
        let n = b.len();
        HwReqs {
            variables: b,
            befores: (0..n).collect(),
            afters: Vec::new(),
            clobbers: Vec::new(),
        }
    }
    pub fn with_afters(a: Vec<HwRange>) -> HwReqs {
        let n = a.len();
        HwReqs {
            variables: a,
            befores: Vec::new(),
            afters: (0..n).collect(),
            clobbers: Vec::new(),
        }
    }
    pub fn push_before(&mut self, hw: HwRange) {
        let i = self.variables.len();
        self.variables.push(hw);
        self.befores.push(i);
    }
    pub fn push_after(&mut self, hw: HwRange) {
        let i = self.variables.len();
        self.variables.push(hw);
        self.afters.push(i);
    }
}

// pieces necessary to call the function, specifically to generate
// the call-site prelude and suffix, once we have
// the actual address of it.
#[derive(Debug,Clone,PartialEq)]
pub struct FunSignature {
    pub argtypes: Vec<DataType>,
    pub return_type: Box<Option<DataType>>, // pub convention: String
}

// if a type is None, that means it's a primitive
pub type GlobalTypeNamespace = HashMap<String, (u64, Option<DataType>)>;

pub fn bigint(n: i64) -> BigInt {
    FromPrimitive::from_i64(n).unwrap()
}
