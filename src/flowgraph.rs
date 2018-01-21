use num::BigInt;
use common::*;
use parsetree::VarDef;
use hw;
struct IndexPool<T> {
    data: Vec<T>
}
impl<T: Default> IndexPool<T> {
    fn new_pool() -> IndexPool<T> {
        IndexPool {
            data: Vec::new()
        }
    }
    fn new_from(&mut self, t: T) -> usize {
        data.push(t);
        data.len() - 1
    }
    fn new(&mut self) -> usize {
        self.new_from(T::default())
    }
    fn get(&self, i: usize) -> &T {
        self.data.get(i)
    }
}


pub struct FlowGraph {
    // these must be typechecked!
    pub statements: IndexPool<Statement>,
    pub vars: IndexPool<Var>,
    pub states: IndexPool<State>,
    //pub types: Vec<DataType>, // temporary values, inputs and outputs for statements
    pub reqs: hw::Reqs, // where inputs and outputs to function go
}

impl FlowGraph {
    pub fn new() -> Self {
        FlowGraph {
            statements: IndexPool::new_pool(),
            vars: IndexPool::new_pool(),
            states: IndexPool::new_pool(),
            reqs: hw::Reqs::new(),
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum Operation {
    Plus,
    CallPtr, // function pointer is first input
    Const,
    CopyOnly,
}

pub struct StatementAction {
    pub op: Operation,
    pub inputs: Vec<ActionInput>,
    pub output: usize, // varidx
}

pub enum ActionInput {
    Var(usize),
    Const(Literal),
}

pub enum Literal {
    Numeric(BigInt),
    Label(String),
}

#[derive(Clone,Debug)]
pub struct Statement {
    pub label: String,
    pub physical_order: Option<usize>,
    pub action: StatementAction,
    pub start_state: usize,
    pub end_state: usize,
    pub exit: Exit,
}
pub struct State {
}

pub enum Exit {
    Goto(JumpTarget),
    Branch(Condition, usize, usize), // the yes and else statements
}

pub enum JumpTarget {
    Statement(usize),
    Ret
}

pub struct Continuation {
    target: JumpTarget,
    state: usizse,
}

enum Condition {
    Eql(ActionInput, ActionInput), Neq(ActionInput, ActionInput),
    Gt(ActionInput, ActionInput), Lte(ActionInput, ActionInput),
    Lt(ActionInput, ActionInput), Gte(ActionInput, ActionInput),
}

