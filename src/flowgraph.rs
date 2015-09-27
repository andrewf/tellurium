use num::BigInt;
use common::*;
use parsetree::VarDef;
// The main data structures returned by typeck functions

// possible location of a value.
// local var, or has to be accessed by asm label
#[derive(Debug)]
enum Location {
    Slot(usize),
    Labeled(String)
}

#[derive(Debug)]
pub enum StmtAction {
    Imm(BigInt), // establish or create an immediate value
    Call(Location, FunSignature), // args, return are in container struct
    Return,
    //Assign,  // to a mem-var
    // assembly
    // condition, loop will contain Vec<Statement>
}

// represents a single executable statement with
// "slot" inputs and outputs. Basically a function call
// with args already evaluated, or other primitive statement
#[derive(Debug)]
pub struct Statement {
    // some sort of content, I guess. looked-up fn, etc
    // something you can directly turn into an assembly snippet
    pub action: StmtAction,
    pub inputs: Vec<usize>,
    pub outputs: Vec<usize>
}

pub struct FlowGraph {
    // these must be typechecked!
    pub stmts: Vec<Statement>, // idea is that for codegen, just go through one at a time
    pub localslots: Vec<DataType> // temporary values, inputs and outputs for statements
}

impl FlowGraph {
    pub fn new() -> Self {
        FlowGraph {
            stmts: Vec::new(),
            localslots: Vec::new()
        }
    }
    fn new_slot(&mut self, t: &DataType) -> usize {
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
