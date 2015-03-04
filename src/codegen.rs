use std::io;
use std::error::FromError;

pub enum CodeGenError {
    Io(io::Error),
    Other(String)
}

impl FromError<io::Error> for CodeGenError {
    fn from_error(e: io::Error) -> Self {
        CodeGenError::Io(e)
    }
}

pub fn mkcgerror<S: ToString>(s: S) -> CodeGenError {
    CodeGenError::Other(s.to_string())
}

//pub trait CodeGenerator {
//    fn emit(TopLevel&) -> Result<(), CodeGenError>;
//}
