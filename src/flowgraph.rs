use num::BigInt;
use common::*;
use parsetree::VarDef;
// The main data structures returned by typeck functions

// possible location of a value.
// local var, or has to be accessed by asm label
#[derive(Debug)]
pub enum NodeInput {
    Slot(usize),
    Labeled(String)
}

#[derive(Debug)]
pub enum NodeAction {
    Imm(BigInt), // establish or create an immediate value
    Call(NodeInput, FunSignature), // args, return are in container struct
    Return,
    Assign(String),  // to a mem-var
    // assembly
    // condition, loop will contain Vec<Node>
}

// represents a single executable statement with
// "slot" inputs and outputs. Basically a function call
// with args already evaluated, or other primitive statement
#[derive(Debug)]
pub struct Node {
    // some sort of content, I guess. looked-up fn, etc
    // something you can directly turn into an assembly snippet
    pub action: NodeAction,
    pub inputs: Vec<NodeInput>,
    pub outputs: Vec<usize>
}

pub struct FlowGraph {
    // these must be typechecked!
    pub stmts: Vec<Node>, // idea is that for codegen, just go through one at a time
    pub localslots: Vec<DataType>, // temporary values, inputs and outputs for statements
    pub argslots: Vec<usize>, // slots that are initialized to graph arguments (overall inputs)
}

impl FlowGraph {
    pub fn new() -> Self {
        FlowGraph {
            stmts: Vec::new(),
            localslots: Vec::new(),
            argslots: Vec::new(),
        }
    }
    pub fn new_slot(&mut self, t: &DataType) -> usize {
        self.localslots.push((*t).clone());
        self.localslots.len() - 1
    }
}

pub struct CheckedProgram {
    pub externs: Vec<String>,
    pub function_definitions: Vec<CheckedFunDef>,
    pub global_vars: Vec<VarDef>
}

pub struct CheckedFunDef {
    pub ld_name: String,
    pub signature: FunSignature,
    pub body: FlowGraph
}
