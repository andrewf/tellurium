
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

enum Expression {
    Sizeof(Box<Expression>),
    FunCall(Box<GenericName>, String, Vec<Box<Expression>>)
}
*/

pub type Expression = String;

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

pub type Block = Vec<Expression>;

#[derive(Debug)]
pub struct FunDef {
    ld_name: String,
    //convention: Option<String>,
    //polymorphic_name: Option<String>,
    args: ArgList,
    return_type: DataType,
    body: Block
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
    ld_name: String,
    datatype: DataType,
    init: Expression
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


