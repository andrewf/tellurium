use std::io;
use std::io::Write;
use parsetree::*;
use codegen::CodeGenError;

pub fn emit_pretty<W: Write>(out: &mut W, tl: &TopLevel) -> Result<(), CodeGenError> {
    for item in tl.iter() {
        match item {
            &TopLevelItem::VarDef(ref v) => {
                try!(emit_vardef(out, v, 0))
            }
            &TopLevelItem::FunDef(ref f) => {
                try!(writeln!(out, "fun {} {:?} -> {:?} {{", f.ld_name, f.args, f.return_type));
                for s in f.body.iter() {
                    try!(emit_stmt(out, s, 4))
                }
                try!(writeln!(out, "}}"))
            }
        }
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

fn emit_vardef<W: Write>(out: &mut W, v: &VarDef, indent: u32) -> Result<(), CodeGenError>
{
    try!(spaces(out, indent));
    try!(writeln!(out, "var {} {:?} = {:?}", v.ld_name, v.datatype, v.init));
    Ok(())
}

fn emit_stmt<W: Write>(out: &mut W, stmt: &Statement, indent: u32) -> Result<(), CodeGenError>
{
    match stmt {
        &Statement::Var(ref v) => {
            emit_vardef(out, v, indent)
        }
        &Statement::Expr(ref e) => {
            emit_expr(out, e, indent)
        }
        &Statement::Return(ref e) => {
            try!(spaces(out, indent));
            try!(writeln!(out, "return:"));
            emit_expr(out, e, indent)
        }
    }
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

