use num::BigInt;

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

#[derive(Debug)]
pub enum DataType {
    Void,
    Pointer(Box<DataType>),
    Array(Expression, Box<DataType>),
    Tuple(Vec<DataType>),
    Named(String)
}

#[derive(Debug)]
pub struct FunDef {
    pub ld_name: String,
    pub argnames: Vec<String>,
    pub argtypes: Vec<DataType>,
    pub return_type: DataType,
    pub body: Block
    //convention: Option<String>,
    //polymorphic_name: Option<String>,
}

impl FunDef {
    pub fn new(n: String,
               argnames: Vec<String>,
               argtypes: Vec<DataType>,
               t: DataType,
               body: Block)
       -> FunDef
   {
        FunDef {
            ld_name: n,
            argnames: argnames,
            argtypes: argtypes,
            return_type: t,
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

