use std::io;
use std::fmt;
use std::error;
use std::io::Write;
use std::convert::From;

use common::*;
use flowgraph::CheckedProgram;
use hw;

#[derive(Debug)]
pub enum CodeGenError {
    Io(io::Error),
    Other(String),
}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "codegen error: {:?}", self)
    }
}

impl error::Error for CodeGenError {
    fn description(&self) -> &str {
        //&format!("{:?}", self)
        "codegen error"
    }
    fn cause(&self) -> Option<&error::Error> {
        None
    }
}



impl From<io::Error> for CodeGenError {
    fn from(e: io::Error) -> Self {
        CodeGenError::Io(e)
    }
}

impl From<Error> for CodeGenError {
    fn from(e: Error) -> Self {
        CodeGenError::Other(format!("during codegen: {}", e.msg))
    }
}

pub fn mkcgerr<S: ToString + ?Sized, T>(s: &S) -> Result<T, CodeGenError> {
    Err(CodeGenError::Other(s.to_string()))
}

pub trait CallingConvention {
    fn get_hwreqs(&self, sig: &FunSignature) -> Result<hw::Reqs, Error>;
    fn codegen_prelude(&self, sig: &FunSignature) -> Result<String, Error> {
        Ok("; no prelude".into())
    }
    fn codegen_postlude(&self, sig: &FunSignature) -> Result<String, Error>;
}

// stuff everyone needs to know about the native platform
// well, not so much the parser, but the type checker.
pub trait Platform {
    // these are types that the hardware knows how to operate on
    // names are part of the protocol between platform and typechecker
    fn get_basic_types(&self) -> GlobalTypeNamespace;

    fn get_pointer_type(&self, _: &DataType) -> Result<String, Error>;

    // convention and so forth
    fn get_calling_convention(&self, name: &Option<String>) -> Result<&CallingConvention, Error>;
    //fn get_fun_hwreqs(&self, sig: &FunSignature) -> Result<hw::Reqs, Error>;

    fn codegen(&self, out: &mut Write, prog: CheckedProgram) -> Result<(), CodeGenError>;
}
