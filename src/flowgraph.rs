use num::BigInt;
use common::*;
use parsetree::VarDef;
// The main data structures returned by typeck functions

// possible location of a value.
// either an input or output of a Node
//#[derive(Debug)]
//pub struct NodeInput {
//    slot: Option<usize>,
//    hw: HwRange,
//}
//
//impl NodeInput {
//    fn from_slot(s: usize) -> NodeInput {
//        NodeInput {
//            slot: Some(s),
//            hw: HwRange::new()
//        }
//    }
//    fn from_label(s: String) -> NodeInput {
//        NodeInput {
//            slot: None,
//            hw: HwLoc::Label(s).into()
//        }
//    }
//}

//pub struct NodeOutput {
//    slot: usize,
//    hw: HwRange,
//}

//impl NodeOuput {
//    fn from_slot(s: usize) -> NodeOutput {
//        NodeOutput {
//            slot: s,
//            hw: HwRange::new()
//        }
//    }
////    fn from_imm(
//}
//
//
//// usize is basically an index into graph.types, indicates connections
//// between Nodes. HwRange is the range of HwLocs in which the Node
//// can accept that data.
//pub struct Edge(usize, HwRange);
//
//impl Edge {
//    fn from_slot(s: usize) -> Edge {
//        Edge(s, None)
//    }
//    fn get_slot(&self) -> usize {
//        let Edge(s, _) = self;
//        s
//    }
//}

#[derive(Debug)]
pub enum NodeAction {
    Call(FunSignature), // callee, args, return are in container struct
    CopyOnly,  // only intended action is copies generated to satisfy this nodes hwloc requirements
               // could be immediate value, global store, or explicit load
    Return,
    //Assign(String),  // to a mem-var
    // assembly
    // condition, loop will contain Vec<Node>
}

// represents a single executable statement with
// "slot" inputs and outputs. Basically a function call
// with args already evaluated, or other primitive statement
#[derive(Debug)]
pub struct Node {
    pub action: NodeAction,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>,
    pub hwreqs: HwReqs, // any restrictions on where the slot inputs and outputs are located
}

// given a HwLoc, make a node with that hwloc as its output
// mainly useful for loading/introducing immediates and global vars
pub fn node_from_hw(hw: HwLoc, slot: usize) -> Node {
    let mut reqs = HwReqs::new();
    reqs.afters.push(hw.into());
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
    pub reqs: HwReqs,  // where inputs and outputs to function go
}

impl FlowGraph {
    pub fn new() -> Self {
        FlowGraph {
            nodes: Vec::new(),
            localslots: Vec::new(),
            reqs: HwReqs::new(),
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
    pub body: FlowGraph
}

pub struct CheckedProgram {
    pub externs: Vec<String>,
    pub function_definitions: Vec<CheckedFunDef>,
    pub global_vars: Vec<VarDef>
}

