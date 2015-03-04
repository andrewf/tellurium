use std::io::Write;
use parsetree::*;
use codegen::CodeGenError;

pub fn emit_pretty<W: Write>(out: &mut W, tl: &TopLevel) -> Result<(), CodeGenError> {
    try!(writeln!(out, "hey there {} {}", tl.vars.len(), tl.funs.len()));
    for v in tl.vars.iter() {
        try!(writeln!(out, "var {} {:?} = {:?}", v.ld_name, v.datatype, v.init))
    }
    for f in tl.funs.iter() {
        try!(writeln!(out, "fun {} {:?} -> {:?} {:?}", f.ld_name, f.args, f.return_type, f.body))
    }
    Ok(())
}

