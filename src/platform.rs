use std::io;
use std::io::Write;
use std::convert::From;

use common::*;
use flowgraph::CheckedProgram;

#[derive(Debug)]
pub enum CodeGenError {
    Io(io::Error),
    Other(String)
}
impl From<io::Error> for CodeGenError {
    fn from(e: io::Error) -> Self {
        CodeGenError::Io(e)
    }
}
pub fn mkcgerr<S: ToString + ?Sized, T>(s: &S) -> Result<T, CodeGenError> {
    Err(CodeGenError::Other(s.to_string()))
}

// stuff everyone needs to know about the native platform
// well, not so much the parser, but the type checker.
pub trait Platform {
    // these are types that the hardware knows how to operate on
    // names are part of the protocol between platform and typechecker
    fn get_basic_types(&self) -> GlobalTypeNamespace;

    fn get_pointer_type(&self, _: &DataType) -> Result<String, Error>;

    // convention and so forth
    fn get_fun_hwreqs(&self, sig: &FunSignature) -> Result<HwReqs, Error>;

    fn codegen(&self, out: &mut Write, prog: CheckedProgram)
        -> Result<(), CodeGenError>;
}

