
/*
enum LValue {
    Name(String),
    Tuple(Vec<LValue>)
}

// LValue with optional  type declaration
enum LValueDecl {
    Name(String, Option<DataType>),
    Tuple(Vec<LValueDecl>, Option<DataType>)
}

enum GenericArg {
    Value(Expression),
    Type(DataType)
}

struct GenericName {
    name: String//,
    // will add this when generics are actually supported
    //args: Option<Vec<Box<GenericArg>>>
}
*/

#[derive(Debug)]
pub enum Expression {
    Ident(String),
    FunCall(Box<Expression>, Vec<Expression>),
    Literal(String), // numeric
    Assign(Box<Expression>, Box<Expression>),
    Subscript(Box<Expression>, Box<Expression>),
    Dot(Box<Expression>, String),
    Array(Vec<Expression>, bool),  // bool is whether last element should be continued
    PtrDeref(Box<Expression>),
    Address(Box<Expression>),
    StrLit(String)
}

pub type GenericName = String;

#[derive(Debug)]
pub enum DataType {
    Void,
    Pointer(Box<DataType>),
    Array(Expression, Box<DataType>),
    //Tuple(Vec<DataType>),
    Named(GenericName) // plain strings go here
}                      // we can tell from context that it's a type

pub type ArgList = Vec<(String, DataType)>;

#[derive(Debug)]
pub enum Statement {
    Expr(Expression),
    Var(VarDef),
    Return(Expression)
}

pub type Block = Vec<Statement>;

#[derive(Debug)]
pub struct FunDef {
    pub ld_name: String,
    //convention: Option<String>,
    //polymorphic_name: Option<String>,
    pub args: ArgList,
    pub return_type: DataType,
    pub body: Block
}

impl FunDef {
    pub fn new(n: String, args: ArgList, t: DataType, body: Block) -> FunDef {
        FunDef {
            ld_name: n,
            args: args,
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

