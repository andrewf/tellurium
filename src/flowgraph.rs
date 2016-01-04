use num::BigInt;
use common::*;
use parsetree::VarDef;
use hw;

#[derive(Clone,Debug,PartialEq)]
pub enum NodeAction {
    Call(FunSignature), // callee, args, return are in container struct
    // callee is last element of befores
    CopyOnly, // only intended action is copies generated to satisfy
    // this node's hwloc requirements
    // could be immediate value, global store, or explicit load
    Return,
    //Assembly(code),
}

// represents a single executable statement with
// "slot" inputs and outputs. Basically a function call
// with args already evaluated, or other primitive statement
#[derive(Clone,Debug)]
pub struct Node {
    pub action: NodeAction,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
    pub hwreqs: hw::Reqs, // any restrictions on where the slot inputs and outputs are located
}

// given a hw::Loc, make a node with that hwloc as its output
// mainly useful for loading/introducing immediates and global vars
pub fn node_from_hw(hw: hw::Loc, slot: usize) -> Node {
    let mut reqs = hw::Reqs::new();
    reqs.push_after(hw.into());
    Node {
        action: NodeAction::CopyOnly,
        inputs: Vec::new(),
        outputs: vec![slot],
        hwreqs: reqs,
    }
}

pub struct FlowGraph {
    // these must be typechecked!
    pub nodes: Vec<Node>,
    pub localslots: Vec<DataType>, // temporary values, inputs and outputs for statements
    pub reqs: hw::Reqs, // where inputs and outputs to function go
}

impl FlowGraph {
    pub fn new() -> Self {
        FlowGraph {
            nodes: Vec::new(),
            localslots: Vec::new(),
            reqs: hw::Reqs::new(),
        }
    }
    pub fn new_slot(&mut self, t: &DataType) -> usize {
        self.localslots.push((*t).clone());
        self.localslots.len() - 1
    }
}

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
