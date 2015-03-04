#![feature(box_syntax)]
#![feature(collections)]
#![feature(fs)]
#![feature(io)]

use std::char::CharExt;
use std::fs::File;
use std::io::Read;
use std::io::stdout;

mod lexer;
mod parsetree;
mod prettycode;
mod codegen;

#[macro_use]
mod recdec;

use lexer::Token;
use recdec::*;
use parsetree::*;

fn is_ident<'a>(t: &Token<'a>) -> bool {
    t.text.char_at(0).is_alphabetic()
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, String> {
    parse!(tok = expect(tokens, "Expected identifier", is_ident));
    // later, maybe check it's not a reserved word
    Ok((tokens, tok.text.to_string()))
}

// expr = ident | ident ( expr ) | number
fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    match ident(tokens) {
        Ok((tokens, id)) => {
            // maybe parens?
            if peek_pred(tokens, &|t| t.text == "(") {
                //println!("fncall");
                let mut args = Vec::new();
                let mut tokens = &tokens[1..]; // move on
                while !peek_pred(tokens, &|t| t.text == ")") {
                tokens = {
                        // argument
                        parse!(arg = expr(tokens));
                        //println!(" arg: {:?}", arg);
                        args.push(arg);
                        if peek_pred(tokens, &|t| t.text == ",") {
                            //println!("  next arg");
                            ignore(tokens, |t| t.text == ",")
                        } else if peek_pred(tokens, &|t| t.text == ")") {
                            tokens // don't change anything else
                        } else {
                            return Err(ParseError::new("Exected , or ) after argument",
                                                       tokens))
                        }
                    }
                }
                //println!("arg loop finished");
                parse!(_ = expect(tokens, "expected ) after args", |t| t.text == ")"));
                //println!("no more args");
                return Ok((tokens, Expression::FnCall(id, args)))
            } else {
                // just a name
                Ok((tokens, Expression::Ident(id)))
            }
        }
        Err(_) => {
            // maybe number literal?
            parse!(lit = expect(tokens,
                                "Expected literal expression",
                                |t| { t.text.char_at(0).is_numeric() }));
            Ok((tokens, Expression::Literal(lit.text.to_string())))
        }
    }
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Block> {
    parse!(_ = expect_word(tokens, "Block must start with {", "{"));
    let mut ret = Vec::new();
    let mut tokens = eatnewlines(tokens);
    while tokens.len() > 0 && !peek_pred(tokens, &|t| t.text == "}") {
        let t = tokens; // alias for macro
        //println!("looking for body expr");
        parse!(e = expr(t));
        //println!("  got expr {:?}", e);
        ret.push(e);
        tokens = eatnewlines(t);
    }
    parse!(_ = expect_word(tokens, "Block must end with }", "}"));
    Ok((tokens, ret))
}

fn datatype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, DataType> {
    if peek_pred(tokens, &|t| { t.text == "ptr" }) {
        // ptr to ...
        parse!(_ = expect_word(tokens, "I just checked this, seriously", "ptr"));
        parse!(referrent = datatype(tokens));
        Ok((tokens, DataType::Pointer(box referrent)))
    } else if peek_pred(tokens, &|t| { t.text == "["}) {
        // array of...
        parse!(_ = expect_word(tokens, "just checked for opening bracket", "["));
        parse!(size = expr(tokens));
        parse!(_ = expect_word(tokens, "Expected ] after array size", "]"));
        parse!(referrent = datatype(tokens));
        Ok((tokens, DataType::Array(size, box referrent)))
    } else {
        // just a name
        parse!(name = ident(tokens));
        Ok((tokens, DataType::Named(name)))
    }
}

fn parse_arglist<'a>(tokens: Cursor<'a>) -> ParseResult<'a, ArgList> {
    let mut ret = Vec::new();
    let (mut tokens, _) = try!(expect_word(tokens, "expected ( at start of params", "("));
    while !peek_pred(tokens, &|t| {t.text ==  ")"}) {
        let t = tokens; // rename it so macro can redefine it, while keeping mut version
        parse!(name = ident(t));
        parse!(dt = datatype(t)); // todo parse types
        ret.push((name, dt)); // store them in list
        // break if no comma
        match expect_word(t, "expected , between params", ",") {
            Ok((t_after_comma, _)) => {
                tokens = t_after_comma
            }
            Err(_) => {
                tokens = t;
                break;
            }
        }
    }
    parse!(_ = expect_word(tokens, "expected ) after params", ")"));
    Ok((tokens, ret))
}

fn fundef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, FunDef> {
    parse!(_  = expect_word(tokens, "um, want a function", "fun"));
    //let (tokens, name) = try!(ident(tokens));
    parse!(name = ident(tokens));
    parse!(args = parse_arglist(tokens));
    // maybe return type
    let (tokens, ret_type) =
        match expect(tokens, "", |t| { t.text == "->" }) {
            Ok((zzz, _)) => {
                try!(datatype(zzz))
            }
            _ => (tokens, DataType::Void)
        };
    // body
    parse!(body = block(tokens));
    Ok((tokens, FunDef::new(name, args, ret_type, body)))
}

fn vardef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, VarDef> {
    parse!(_ = expect_word(tokens, "expected 'var'", "var"));
    parse!(name = ident(tokens));
    parse!(t = datatype(tokens));
    parse!(_ = expect_word(tokens, "variable must be initialized with =", "="));
    parse!(e = expr(tokens));
    parse!(_ = expect_word(tokens, "expected newline after var", "\n"));
    Ok((tokens, VarDef::new(name, t, e)))
}

// turns out it's tricky to make this a closure
fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.text == "\n"
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn toplevel<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, TopLevel> {
    let mut tl = TopLevel::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        if peek_pred(tokens, &|t| t.text == "fun") {
            let (newtok, f) = try!(fundef(tokens));
            tokens = newtok;
            tl.funs.push(f);
        } else if peek_pred(tokens, &|t| t.text == "var") {
            let (newtok, v) = try!(vardef(tokens));
            tokens = newtok;
            tl.vars.push(v)
        }
        tokens = eatnewlines(tokens);
    }
    Ok((tokens, tl))
}

fn main() {
    let words = ["->", "%%", "(", ")", "[", "]", "\n",
                "<", ">", "-", "+", "*", "/", "@", "&",
                "!", "$", "{", "}", "%", ",", "=", "#",
                "^", "~", "|", ":", ";", ".", "_", "\\"];
    // load a file
    let mut programtext = String::new();
    File::open("orig.te").ok().unwrap().read_to_string(&mut programtext).ok().unwrap();
    let tokens = lexer::lex(&programtext[..], &words);
    match tokens {
        Ok(tokens) => {
            //for t in tokens.iter() {
            //    println!(
            //        "({}:{}): {:?}",
            //        t.line, t.column, t.text
            //    )
            //}
            let parsed = toplevel(&tokens[..]);
            println!("{:?}", toplevel(&tokens[..]));
            match parsed {
                Ok((_, tl)) => {
                    prettycode::emit_pretty(&mut stdout(), &tl);
                },
                Err(_) => {}
            }
        }
        Err(tok) => {
            println!("Lexer error. unexpected token {:?}", tok)
        }
    }
}
