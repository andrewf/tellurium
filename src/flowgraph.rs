use num::BigInt;
use parsetree::FunSignature;
// The main data structures returned by typeck functions

// possible location of a value.
// local var, or has to be accessed by asm label
enum Location {
    Slot(usize),
    Labeled(String)
}

enum StmtAction {
    Call(Location, FunSignature), // args, return in container struct
    Assign,  // to a mem-var
    Return,
    Imm(BigInt)  // establish or create an immediate value
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
    inputs: Vec<usize>,
    outputs: usize
}



struct FlowGraph {
    // these must be typechecked!
    stmts: Vec<Statement>, // idea is that for codegen, just go through one at a time
    localslots: Vec<DataType> // temporary values, inputs and outputs for statements
}

impl FlowGraph {
    fn new_slot(&mut self, t: &DataType) -> usize {
        self.localslots.push(t.clone());
        self.localslots.len() - 1
    }
}
