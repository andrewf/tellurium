use std::io;
use std::io::Write;
use parsetree::*;
use codegen::CodeGenError;

pub fn emit_pretty<W: Write>(out: &mut W, tl: &TopLevel) -> Result<(), CodeGenError> {
    for v in tl.vars.iter() {
        try!(writeln!(out, "var {} {:?} = {:?}", v.ld_name, v.datatype, v.init))
    }
    for f in tl.funs.iter() {
        try!(writeln!(out, "fun {} {:?} -> {:?} {{", f.ld_name, f.args, f.return_type));
        for e in f.body.iter() {
            try!(emit_expr(out, &e, 4))
        }
        try!(writeln!(out, "}}"))
    }
    Ok(())
}

fn spaces<W: Write>(out: &mut W, mut indent: u32) -> io::Result<()> {
    let space = [' ' as u8];
    while indent > 0 {
        let _  = try!(out.write(&space));
        indent = indent - 1
    }
    Ok(())
}
    

fn emit_expr<W: Write>(out: &mut W, expr: &Expression, indent: u32) -> Result<(), CodeGenError> {
    try!(spaces(out, indent));
    match expr {
        &Expression::Ident(ref s) => try!(writeln!(out, "{}", *s)),
        &Expression::Literal(ref s) => try!(writeln!(out, "{}", *s)),
        &Expression::FnCall(ref s, ref more) => {
            try!(writeln!(out, "{}(", *s));
            for e in more.iter() {
                try!(emit_expr(out, &e, indent + 4));
            }
            try!(spaces(out, indent));
            try!(writeln!(out, ")"))
        }
    };
    Ok(())
}

