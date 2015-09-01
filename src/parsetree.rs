use num::BigInt;
use tuplity::Tuplity;

#[derive(Debug)]
pub enum Expression {
    Ident(String),
    FunCall(Box<Expression>, Vec<Expression>),
    Literal(BigInt), // numeric
    Assign(Box<Expression>, Box<Expression>),
    Subscript(Box<Expression>, Box<Expression>),
    Dot(Box<Expression>, String),
    Tuple(Vec<Expression>),
    Array(Vec<Expression>, bool),  // bool is whether last element should be continued
    PtrDeref(Box<Expression>),
    Address(Box<Expression>),
    StrLit(String)
}

#[derive(Debug)]
pub enum Statement {
    Expr(Expression),
    Var(VarDef),
    Return(Expression),
    Condition(Expression, Block, Option<Block>)
}

pub type Block = Vec<Statement>;

// pieces necessary to call the function, once we have
// the actual address of it.
#[derive(Debug)]
pub struct FunSignature {
    pub argtypes: Vec<DataType>,
    pub return_type: Box<Tuplity<DataType>>
    //pub convention: String
}

impl FunSignature {
    fn new(a: Vec<DataType>, r: DataType) -> FunSignature {
        FunSignature {
            argtypes: a,
            return_type: box r
        }
    }
}

#[derive(Debug)]
pub enum DataType {
    Pointer(Box<DataType>),
    Array(Expression, Box<DataType>),
    Named(String),
    Fun(FunSignature)
}

#[derive(Debug)]
pub struct FunDef {
    pub ld_name: String,
    pub signature: FunSignature,
    pub argnames: Vec<String>,
    pub body: Block
    //polymorphic_name: Option<String>,
}

impl FunDef {
    pub fn new(n: String,
               argnames: Vec<String>,
               argtypes: Vec<DataType>,
               ret: DataType,
               body: Block)
       -> FunDef
    {
        FunDef {
            ld_name: n,
            signature: FunSignature::new(argtypes, ret),
            argnames: argnames,
            body: body
        }
    }
}

#[derive(Debug)]
pub struct VarDef {
    pub ld_name: String,
    pub datatype: DataType,
    pub init: Expression
}

impl VarDef {
    pub fn new(n: String, t: DataType, init: Expression) -> VarDef {
        VarDef {
            ld_name: n,
            datatype: t,
            init: init
        }
    }
}

#[derive(Debug)]
pub enum TopLevelItem {
    VarDef(VarDef),
    FunDef(FunDef)
    // Generic(TopLevel)
}

pub type TopLevel = Vec<TopLevelItem>;

