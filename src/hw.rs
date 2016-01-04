use num::BigInt;
use num::FromPrimitive;

// place you can put a local variable
#[derive(Debug,Clone,PartialEq)]
pub enum Loc {
    Register(String), // name without any % or $
    Label(String), // essentially an address literal. wrap in Mem to get value here
    Imm(BigInt), // immediate/constant value
    // The address of the top of the stack
    StackPtr, // generally used with Mem
    Mem(Box<Loc>, i64), // location to deref, offset
}

impl Loc {
    pub fn labelled_var<S: ToString + ?Sized>(s: &S) -> Loc {
        Loc::Mem(box Loc::Label(s.to_string()), 0)
    }
    pub fn from_regname<S: ToString + ?Sized>(s: &S) -> Loc {
        Loc::Register(s.to_string())
    }
    pub fn stack(offset: i64) -> Loc {
        Loc::Mem(box Loc::StackPtr, offset)
    }
    pub fn from_int(val: i64) -> Loc {
        Loc::Imm(FromPrimitive::from_i64(val).unwrap())
    }
}

// Set of possibilities for the hardware location of an input or
// output to a stmt
// For now, either a specific location, or no restriction
#[derive(Debug,Clone,PartialEq)]
pub struct Range(Option<Loc>); // so hack. much gross.

impl Range {
    pub fn new() -> Range {
        Range(None)
    }
    // if the range's only possible value is a single hwloc, return that
    // else return None
    pub fn concrete(&self) -> Option<&Loc> {
        if let &Range(Some(ref loc)) = self {
            Some(loc)
        } else {
            None
        }
    }
}

impl From<Loc> for Range {
    fn from(h: Loc) -> Range {
        Range(Some(h))
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Reqs {
    // store one master list of hardware variables,
    // into which the other members store indices
    // this enables the expression of requirements that,
    // e.g. an input be the same as an output.
    pub variables: Vec<Range>,
    pub befores: Vec<usize>,
    pub afters: Vec<usize>,
    pub clobbers: Vec<usize>, // TODO ensure freedom from conflict within variables
}

impl Reqs {
    pub fn new() -> Reqs {
        Reqs {
            variables: Vec::new(),
            befores: Vec::new(),
            afters: Vec::new(),
            clobbers: Vec::new(),
        }
    }
    pub fn with_befores(b: Vec<Range>) -> Reqs {
        let n = b.len();
        Reqs {
            variables: b,
            befores: (0..n).collect(),
            afters: Vec::new(),
            clobbers: Vec::new(),
        }
    }
    pub fn with_afters(a: Vec<Range>) -> Reqs {
        let n = a.len();
        Reqs {
            variables: a,
            befores: Vec::new(),
            afters: (0..n).collect(),
            clobbers: Vec::new(),
        }
    }
    pub fn push_before(&mut self, hw: Range) {
        let i = self.variables.len();
        self.variables.push(hw);
        self.befores.push(i);
    }
    pub fn push_after(&mut self, hw: Range) {
        let i = self.variables.len();
        self.variables.push(hw);
        self.afters.push(i);
    }
}


