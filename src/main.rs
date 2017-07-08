#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(plugin)]
extern crate regex;

extern crate num;

use std::fs::File;
use std::io::Read;
use std::io::stdout;
use std::env::args_os;
use std::process;
use num::BigInt;

mod lexer;
mod common;
mod parsetree;
mod flowgraph;
mod codegen;
mod typeck;
mod platform;
mod hw;

#[macro_use]
mod recdec;

use common::*;
use recdec::*;
use recdec::ParseStatus::*;
use parsetree::*;
use num::traits::ToPrimitive;  // to_u64
use lexer::LexSpec;
use lexer::LexSpec::*;
use platform::Platform;
use codegen::IntelPlatform;

#[derive(Copy,PartialEq)]
#[derive(Clone,Debug)]
enum TeToken {
    Whitespace,
    Word,
    NumLit,
    StrLit,
    Operator,
}
use TeToken::*;

type Token<'a> = lexer::Token<'a, TeToken>;
type Cursor<'a> = &'a [Token<'a>];
// single-token parsers should only return NoGo, shouldn't use
// expect

static RESERVED_WORDS: &'static [&'static str] = &["fun", "etc", "do", "end", "if", "then",
                                                   "else", "var", "ptr", "return", "asm", "or",
                                                   "and", "goto", "break", "class", "instance",
                                                   "generic", "type", "extern", "concrete",
                                                   "alias", "as", "struct", "section", "sizeof",
                                                   "length", "module"];

fn regex(pat: &str) -> regex::Regex {
    regex::Regex::new(pat).expect("invalid regex wat?")
}

fn toplevel<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, TopLevel> {
    let mut tl = TopLevel::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        let newtok = tokens;
        parse!(item = toplevelitem(newtok) || mkfail("expected fun or var def at top level"));
        tokens = newtok;
        tl.push(item);
        tokens = eatnewlines(tokens);
    }
    succeed(tokens, tl)
}

fn toplevelitem<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, TopLevelItem> {
    alt!(vardef(tokens), |v| TopLevelItem::VarDef(v));
    alt!(fundef(tokens), |f| TopLevelItem::FunDef(f));
    alt!(externdef(tokens), |e| TopLevelItem::ExternDef(e));
    nogo(tokens)
}

fn protocol<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    parse!(_ = expect_word(tokens, "!") || NoGo(()));
    parse!(proto = ident(tokens) || mkfail("expected identifier after '!'"));
    succeed(tokens, proto)
}

fn fundef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, FunDef> {
    parse!(_  = expect_word(tokens, "fun") || NoGo(()));
    let (tokens, proto) = maybeparse!(protocol(tokens));
    parse!(name = ident(tokens) || mkfail("expected function name"));
    parse!((names, types) = parse_arglist(tokens) || mkfail("expected arglist"));
    // maybe return type
    let (tokens, ret_type) = match expect(tokens, |t| t.text == "->") {
        ParseResult(tokens, Good(_)) => {
            parse!(t = datatype(tokens) || mkfail("expected type after ->"));
            (tokens, Some(t))
        }
        _ => (tokens, None),
    };
    // body
    parse!(body = block(tokens) || mkfail("expected function body"));
    succeed(tokens,
            FunDef {
                ld_name: name,
                signature: FunSignature {
                    argtypes: types,
                    return_type: box ret_type,
                    convention: proto,
                },
                argnames: names,
                body: body,
            })
}

fn parse_arglist<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, (Vec<String>, Vec<DataType>)> {
    let mut names = Vec::new();
    let mut types = Vec::new();
    parse!(_ = expect(tokens, |t| t.text == "(") || NoGo(()));
    let mut tokens = tokens;
    // let (mut tokens, _) = try!(expect_word(tokens, "expected ( at start of params", "("));
    while !peek_pred(tokens, &|t| t.text == ")") {
        let t = tokens; // rename it so macro can redefine it, while keeping mut version
        parse!(name = ident(t) || mkfail("expected param name"));
        parse!(dt = datatype(t) || mkfail("expected param type")); // todo parse types
        // store them in their respective lists
        names.push(name);
        types.push(dt);
        // break if no comma
        match expect_word(t, ",") {
            ParseResult(t_after_comma, Good(_)) => tokens = t_after_comma,
            _ => {
                tokens = t;
                break;
            }
        }
    }
    parse!(_ = expect_word(tokens, ")") || mkfail("expected ) after params"));
    succeed(tokens, (names, types))
}

fn externdef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, ExternDef> {
    parse!(_ = expect_word(tokens, "extern") || NoGo(()));
    parse!(name = ident(tokens) || mkfail("expected name of external object thingy"));
    parse!(t = datatype(tokens) || mkfail("expected type of external object thingy"));
    succeed(tokens,
            ExternDef {
                ld_name: name,
                datatype: t,
            })
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    match expect(tokens, is_ident) {
        ParseResult(t, Good(tok)) => succeed(t, tok.text.to_string()),
        ParseResult(t, _) => nogo(t),
    }
}

fn vardef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, VarDef> {
    parse!(_ = expect_word(tokens, "var") || NoGo(()));
    parse!(name = ident(tokens) || mkfail("expected variable name"));
    parse!(t = datatype(tokens) || mkfail("expected variable type"));
    parse!(_ = expect_word(tokens, "=") || mkfail("variable must be initialized with ="));
    parse!(e = expr(tokens) || mkfail("expected initialization expression after ="));
    // parse!(_ = expect_word(tokens, "\n") || mkfail("expected newline after var"));
    succeed(tokens, VarDef::new(name, t, e))
}

fn datatype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    alt!(ptrtype(tokens), |t| t);
    alt!(arraytype(tokens), |t| t);
    alt!(ident(tokens), |s| DataType::Basic(s));
    alt!(funtype(tokens), |t| t);
    // alt!(tupletype(tokens), |elems| DataType::Tuple(elems));
    nogo(tokens)
}

fn ptrtype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "ptr") || NoGo(()));
    parse!(referrent = datatype(tokens) || mkfail("ptr must be followed by type"));
    succeed(tokens,
            DataType::Composite(CompositeType::Pointer(box referrent)))
}

fn arraytype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    parse!(size = expr(tokens) || mkfail("expected size expression"));
    let size = match size {
        Expression::Literal(bigsize) => bigsize.to_u64().unwrap(),
        _ => return parsefail(tokens, "array sizes must be literals"),
    };
    parse!(_ = expect_word(tokens, "]") || mkfail("expected ] after size"));
    parse!(elemtype = datatype(tokens) || mkfail("expected array element type"));
    succeed(tokens,
            DataType::Composite(CompositeType::Array(size, box elemtype)))
}

fn returntype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "->") || NoGo(()));
    parse!(t = datatype(tokens) || mkfail("expected type after '->'"));
    succeed(tokens, t)
}

fn funtype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "fun") || NoGo(()));
    parse!(_ = expect_word(tokens, "(") || mkfail("expected '(' after 'fun'"));
    parse!(types = sep(tokens, datatype, |tokens| expect_word(tokens, ","), false) ||
                   mkfail("error parsing fun arg types"));
    parse!(_ = expect_word(tokens, ")") || mkfail("expected ')' after 'fun(<arg types>'"));
    let (tokens, ret_type) = maybeparse!(returntype(tokens));
    succeed(tokens,
            DataType::Composite(CompositeType::Fun(FunSignature {
                argtypes: types,
                return_type: box ret_type,
                convention: None,
            })))
}


// fn tupletype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Vec<DataType>> {
//    parse!(_ = expect_word(tokens, "(") || NoGo(()));
//    parse!(elems = sep(tokens, datatype, |tokens| expect_word(tokens, ","), false)
//            || mkfail("expected tuple contents"));
//    parse!(_ = expect_word(tokens, ")") || mkfail("expected ')' after tuple"));
//    succeed(tokens, elems)
// }

fn statements<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Vec<Statement>> {
    let tokens = eatnewlines(tokens);
    parse!(stmts = sep(tokens,
                       stmt,
                       |tokens| {
                           parse!(_ = expect_word(tokens, "\n") || NoGo(()));
                           succeed(tokens, ())
                       },
                       true) ||
                   mkfail("expected statements, you shouldn't see this message"));
    let tokens = eatnewlines(tokens);
    succeed(tokens, stmts)
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Block> {
    parse!(_ = expect_word(tokens, "do") || NoGo(()));
    parse!(stmts = statements(tokens) || mkfail("invalid block or something"));
    parse!(_ = expect_word(tokens, "end") || mkfail("Block must end with 'end'"));
    succeed(tokens, stmts)
}

fn stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Statement> {
    alt!(return_stmt(tokens), |e| Statement::Return(e));
    alt!(vardef(tokens), |v| Statement::Var(v));
    alt!(condition(tokens),
         |(e, b, elseb)| Statement::Condition(e, b, elseb));
    // expr must be last, since it's the broadest
    alt!(expr(tokens), |e| Statement::Expr(e));
    nogo(tokens)
}

fn return_stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect(tokens, |t| t.text == "return") || NoGo(()));
    expr(tokens)
}

fn condition<'a>(tokens: Cursor<'a>)
                 -> ParseResult<'a, Token<'a>, (Expression, Block, Option<Block>)> {
    parse!(_ = expect_word(tokens, "if") || NoGo(()));
    parse!(e = expr(tokens) || mkfail("expected condition expression after 'if'"));
    parse!(_ = expect_word(tokens, "then") || mkfail("expected 'then' after condition"));
    parse!(then = statements(tokens) || mkfail("expected block after condition"));
    let (tokens, elseblock): (Cursor<'a>, Option<Block>) = maybeparse!(else_clause(tokens));
    parse!(_ = expect_word(tokens, "end") || mkfail("condition block must have 'end' at end"));
    succeed(tokens, (e, then, elseblock))
}

// parsing of "end" is left to condition function
fn else_clause<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Block> {
    parse!(_ = expect_word(tokens, "else") || NoGo(()));
    parse!(b = statements(tokens) || mkfail("expected block after 'else'"));
    succeed(tokens, b)
}

// expr = number | ident after_ident
fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    alt!(num_literal(tokens), |b: BigInt| Expression::Literal(b));
    alt!(ptr_deref(tokens), |e| Expression::PtrDeref(box e));
    alt!(address_expr(tokens), |e| Expression::Address(box e));
    alt!(array_expr(tokens),
         |(elems, cont)| Expression::Array(elems, cont));
    alt!(strlit_expr(tokens), |t| Expression::StrLit(t));
    alt!(tuple_expr(tokens), |elems| Expression::Tuple(elems));
    alt_tail!(ident(tokens), |t, id| after_ident(t, Expression::Ident(id)));
    nogo(tokens)
}

fn num_literal<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, BigInt> {
    parse!(t = expect(tokens, |t| t.toktype == NumLit) || NoGo(()));
    let b = match t.text.parse() {
        Ok(b) => b,
        Err(_) => return parsefail(tokens, "invalid numeric literal"),
    };
    succeed(tokens, b)
}

fn ptr_deref<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "@") || NoGo(()));
    parse!(e = expr(tokens) || mkfail("expected expression after '@'"));
    succeed(tokens, e)
}

fn address_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "@>") || NoGo(()));
    parse!(e = expr(tokens) || mkfail("expected expression after '@>'"));
    succeed(tokens, e)
}

fn strlit_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    parse!(t = expect(tokens, |t| t.toktype == StrLit) || NoGo(()));
    let l = t.text.len();
    // todo: auto-concat
    if l >= 2 {
        succeed(tokens, t.text[1..l - 1].to_string())
    } else {
        succeed(tokens, String::new())
    }
}

fn array_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, (Vec<Expression>, bool)> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    // comma-separated elements
    parse!(elems = sep(tokens, expr, |tokens| expect_word(tokens, ","), false) ||
                   mkfail("expected contents in array"));
    // if there is something in the array, can accept 'etc' to indicate
    // that last element should be repeated to the end of the array
    let (tokens, cont) = {
        if elems.len() > 0 {
            let (tokens, c) = maybeparse!(expect_word(tokens, "etc"));
            (tokens, c.is_some())
        } else {
            (tokens, false)
        }
    };
    parse!(_ = expect_word(tokens, "]") || mkfail("array must finish with ']'"));
    succeed(tokens, (elems, cont))
}

fn tuple_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Vec<Expression>> {
    parse!(_ = expect_word(tokens, "(") || NoGo(()));
    parse!(elems = sep(tokens, expr, |tokens| expect_word(tokens, ","), false) ||
                   mkfail("expected tuple contents"));
    parse!(_ = expect_word(tokens, ")") || mkfail("expected ')' after tuple"));
    succeed(tokens, elems)
}

// no nogo, optional
fn after_ident<'a>(tokens: Cursor<'a>,
                   id: Expression)
                   -> ParseResult<'a, Token<'a>, Expression, Expression> {
    let id = alt!(equals_tail(tokens, id), |t| t);
    let id = alt!(chainable_follower(tokens, id), |x| x);
    nogo_with(tokens, id)
}

fn chainable_follower<'a>(tokens: Cursor<'a>,
                          base: Expression)
                          -> ParseResult<'a, Token<'a>, Expression, Expression> {
    alt_tail!(funcall_args(tokens),
              |tokens, args| chainable_follower(tokens, Expression::FunCall(box base, args)));
    alt_tail!(subscript_index(tokens),
              |tokens, idx| chainable_follower(tokens, Expression::Subscript(box base, box idx)));
    alt_tail!(dot_syntax(tokens),
              |tokens, mem| chainable_follower(tokens, Expression::Dot(box base, mem)));
    nogo_with(tokens, base)
}

fn equals_tail<'a>(tokens: Cursor<'a>,
                   base: Expression)
                   -> ParseResult<'a, Token<'a>, Expression, Expression> {
    parse!(_ = expect_word(tokens, "=") || NoGo(base));
    parse!(rvalue = expr(tokens) || mkfail("expected expression after '='"));
    // stub
    succeed(tokens, Expression::Assign(box base, box rvalue))
}

fn funcall_args<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Vec<Expression>> {
    parse!(_ = expect_word(tokens, "(") || NoGo(()));
    parse!(args = sep(tokens, expr, |tokens| expect_word(tokens, ","), false) ||
                  mkfail("expected args after '('"));
    parse!(_ = expect_word(tokens, ")") || mkfail("Expected ')' after args"));
    succeed(tokens, args)
}

fn subscript_index<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    parse!(e = expr(tokens) || mkfail("expected expression after '['"));
    parse!(_ = expect_word(tokens, "]") || mkfail("Expected ']' after subscript expression"));
    succeed(tokens, e)
}

fn dot_syntax<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    parse!(_ = expect_word(tokens, ".") || NoGo(()));
    parse!(s = ident(tokens) || mkfail("expected ident after '.'"));
    succeed(tokens, s)
}

fn expect_word<'a>(tokens: &'a [Token<'a>],
                   expected_text: &str)
                   -> ParseResult<'a, Token<'a>, Token<'a>> {
    expect(tokens, |t| t.text == expected_text)
}

fn is_ident(t: &Token) -> bool {
    // check token type, and make sure it's not a reserved word
    if t.toktype != Word {
        false
    } else {
        for &reserved in RESERVED_WORDS.iter() {
            if t.text == reserved {
                return false;
            }
        }
        true
    }
}

// turns out it's tricky to make this a closure
fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.text == "\n"
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn main() {
    let ops = ["@>", "->", "%%", "(", ")", "[", "]", "\n", "<", ">", "-", "+", "*", "/", "@", "&",
               "!", "$", "{", "}", "%", ",", "=", "#", "^", "~", "|", ":", ";", ".", r"\"];
    let ops: Vec<_> = ops.iter().map(|s| LexSpec::Lit(s)).collect();
    // load a file
    let args = args_os().collect::<Vec<_>>();
    if args.len() < 2 {
        panic!("Usage: tellurium <input>");
    }
    let mut programtext = String::new();
    File::open(&args[1]).ok().unwrap().read_to_string(&mut programtext).ok().unwrap();
    // tokenize
    let specs = [(Whitespace, &[Re(regex("^[ \t]+"))][..]),
                 (Operator, &ops[..]),
                 (NumLit, &[Re(regex(r"^[:digit:][xa-fA-F0-9_.]*"))][..]),
                 (StrLit, &[Re(regex("^\"[^\"]*\""))][..]),
                 (Word,
                  &[Re(regex(r"^[\p{Alphabetic}_][\p{Alphabetic}\d_]*"))][..])];
    let tokens = lexer::lex::<TeToken>(&programtext[..], &specs[..]);
    let tokens: Vec<_> = tokens.filter(|t| t.toktype != Whitespace)
                               .collect();
    // parse the file
    let ParseResult(t, parsed) = toplevel(&tokens[..]);
    // check success of parsing
    let plat = codegen::IntelPlatform::new();
    let mut status = 0;
    match parsed {
        Good(tl) => {
            match typeck::check_and_flowgen(tl, &plat) {
                Ok(prog) => {
                    match plat.codegen(&mut std::io::stdout(), prog) {
                        Err(e) => {
                            println!("failed to codegen {:?}", e);
                            status = 1;
                        }
                        _ => {}
                    }
                }
                Err(e) => {
                    println!("compile error: {}", e.msg);
                    status = 1;
                }
            }
        }
        Fail(e) => {
            println!("failed to parse: {:?} at {:?}", e, t[0]);
            status = 1;
        }
        NoGo(_) => {
            println!("How does this even happen?");
            status = 1;
        }
    }
    process::exit(status);
}
