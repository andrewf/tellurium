#![feature(box_syntax)]
#![feature(collections)]

use std::char::CharExt;

mod lexer;

#[macro_use]
mod recdec;

use lexer::Token;
use recdec::*;

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

type Expression = String;

type GenericName = String;

#[derive(Debug)]
enum DataType {
    Void,
    Pointer(Box<DataType>),
    Array(Expression, Box<DataType>),
    //Tuple(Vec<DataType>),
    Named(GenericName) // plain strings go here
}                      // we can tell from context that it's a type

type ArgList = Vec<(String, DataType)>;

type Block = Vec<Expression>;

#[derive(Debug)]
struct FunDef {
    ld_name: String,
    //convention: Option<String>,
    //polymorphic_name: Option<String>,
    args: ArgList,
    return_type: DataType,
    body: Block
}

#[derive(Debug)]
struct VarDef {
    ld_name: String,
    datatype: DataType,
    init: Expression
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, String> {
    //let (cur, tok) = try!(expect_type(tokens, "Expected identifier", TokenType::Word));
    parse!(tok = expect(tokens, "Expected identifier",
                        |t| t.text.char_at(0).is_alphabetic()));
    // later, maybe check it's not a reserved word
    Ok((tokens, tok.text.to_string()))
}

fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    //let (tokens, tok) = try!(expect_type(tokens, "Expected expression", TokenType::Word));
    parse!(tok = expect(tokens, "Expected expression",
                        |t| t.text.char_at(0).is_alphanumeric()));
    Ok((tokens, tok.text.to_string()))
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Block> {
    parse!(_ = expect_word(tokens, "Block must start with {", "{"));
    let mut ret = Vec::new();
    let mut tokens = eatnewlines(tokens);
    while tokens.len() > 0 && !peek_pred(tokens, &|t| t.text == "}") {
        let t = tokens; // alias for macro
        parse!(e = expr(t));
        tokens = t;
        ret.push(e)
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
    Ok((tokens,
        FunDef{
            ld_name: name,
            args: args,
            return_type: ret_type,
            body: body
    }))
}

fn vardef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, VarDef> {
    parse!(_ = expect_word(tokens, "expected 'var'", "var"));
    parse!(name = ident(tokens));
    parse!(t = datatype(tokens));
    parse!(_ = expect_word(tokens, "variable must be initialized with =", "="));
    parse!(e = expr(tokens));
    parse!(_ = expect_word(tokens, "expected newline after var", "\n"));
    Ok((tokens, VarDef{ ld_name: name, datatype: t, init: e }))
}

// turns out it's tricky to make this a closure
fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.text == "\n"
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn toplevel<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, (Vec<VarDef>, Vec<FunDef>)> {
    let mut vars = Vec::new();
    let mut funs = Vec::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        if peek_pred(tokens, &|t| t.text == "fun") {
            let (newtok, f) = try!(fundef(tokens));
            tokens = newtok;
            funs.push(f);
        } else if peek_pred(tokens, &|t| t.text == "var") {
            let (newtok, v) = try!(vardef(tokens));
            tokens = newtok;
            vars.push(v)
        }
        tokens = eatnewlines(tokens);
    }
    Ok((tokens, (vars, funs)))
}

fn main() {
    let words = ["->", "%%", "(", ")", "[", "]", "\n",
                "<", ">", "-", "+", "*", "/", "@", "&",
                "!", "$", "{", "}", "%", ",", "=", "#",
                "^", "~", "|", ":", ";", ".", "_", "\\"];
    let program = "\n\n  \n  fun grup ( a [3]b, c ptr [5] d,) {x y z} var x u16 = 4 \n var y u16 = zap\n\n fun   foo () -> [r]i32 { zap zippie zow}\n";
    println!("starting lexer");
    let tokens = lexer::lex(&program, &words);
    match tokens {
        Ok(tokens) => {
            for t in tokens.iter() {
                println!(
                    "({}:{}): {:?}",
                    t.line, t.column, t.text
                )
            }
            println!("{:?}", toplevel(&tokens[..]))
        }
        Err(tok) => {
            println!("Lexer error. unexpected token {:?}", tok)
        }
    }
}
