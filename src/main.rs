#![feature(box_syntax)]
#![feature(collections)]

#![feature(plugin)]
#![plugin(regex_macros)]
extern crate regex;

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

use recdec::*;
use recdec::ParseStatus::*;
use parsetree::*;
use lexer::LexSpec;
use lexer::LexSpec::*;

#[derive(Copy,PartialEq)]
#[derive(Clone,Debug)]
enum TeToken {
    Whitespace,
    Ident,
    NumLit,
    StrLit,
    Keyword // operators too
}
use TeToken::*;

type Token<'a> = lexer::Token<'a, TeToken>;
type Cursor<'a> = &'a[Token<'a>];
// single-token parsers should only return NoGo, shouldn't use
// expect

fn toplevel<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, TopLevel> {
    let mut tl = TopLevel::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        let newtok = tokens;
        parse!(item = toplevelitem(newtok) || mkerr("expected fun or var def at top level"));
        tokens = newtok;
        tl.push(item);
        tokens = eatnewlines(tokens);
    }
    succeed(tokens, tl)
}

fn toplevelitem<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, TopLevelItem> {
    alt!(vardef(tokens), |v| TopLevelItem::VarDef(v));
    alt!(fundef(tokens), |f| TopLevelItem::FunDef(f));
    nogo(tokens)
}

fn fundef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, FunDef> {
    parse!(_  = expect_word(tokens, "fun") || NoGo(()));
    parse!(name = ident(tokens) || mkerr("expected function name"));
    parse!(args = parse_arglist(tokens) || mkerr("expected arglist"));
    // maybe return type
    let (tokens, ret_type) =
        match expect(tokens, |t| { t.text == "->" }) {
            ParseResult(tokens, Good(_)) => {
                parse!(t = datatype(tokens) || mkerr("expected type after ->"));
                (tokens, t)
            }
            _ => (tokens, DataType::Void)
        };
    // body
    parse!(body = block(tokens) || mkerr("expected function body"));
    succeed(tokens, FunDef::new(name, args, ret_type, body))
}

fn parse_arglist<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, ArgList> {
    let mut ret = Vec::new();
    parse!(_ = expect(tokens, |t| t.text == "(") || NoGo(()));
    let mut tokens = tokens;
    //let (mut tokens, _) = try!(expect_word(tokens, "expected ( at start of params", "("));
    while !peek_pred(tokens, &|t| {t.text ==  ")"}) {
        let t = tokens; // rename it so macro can redefine it, while keeping mut version
        parse!(name = ident(t) || mkerr("expected param name"));
        parse!(dt = datatype(t) || mkerr("expected param type")); // todo parse types
        ret.push((name, dt)); // store them in list
        // break if no comma
        match expect_word(t, ",") {
            ParseResult(t_after_comma, Good(_)) => {
                tokens = t_after_comma
            }
            _ => {
                tokens = t;
                break;
            }
        }
    }
    parse!(_ = expect_word(tokens, ")") || mkerr("expected ) after params"));
    succeed(tokens, ret)
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    match expect(tokens, is_ident) {
        ParseResult(t, Good(tok)) => {
            succeed(t, tok.text.to_string())
        }
        ParseResult(t, _) => {
            nogo(t)
        }
    }
}

fn vardef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, VarDef> {
    parse!(_ = expect_word(tokens, "var") || NoGo(()));
    parse!(name = ident(tokens) || mkerr("expected variable name"));
    parse!(t = datatype(tokens) || mkerr("expected variable type"));
    parse!(_ = expect_word(tokens, "=") || mkerr("variable must be initialized with ="));
    parse!(e = expr(tokens) || mkerr("expected initialization expression after ="));
    //parse!(_ = expect_word(tokens, "\n") || mkerr("expected newline after var"));
    succeed(tokens, VarDef::new(name, t, e))
}

fn datatype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    alt!(ptrtype(tokens), |t| t);
    alt!(arraytype(tokens), |t| t);
    alt!(ident(tokens), |s| DataType::Named(s));
    nogo(tokens)
}

fn ptrtype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "ptr") || NoGo(()));
    parse!(referrent = datatype(tokens) || mkerr("ptr must be followed by type"));
    succeed(tokens, DataType::Pointer(box referrent))
}

fn arraytype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, DataType> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    parse!(size = expr(tokens) || mkerr("expected size expression"));
    parse!(_ = expect_word(tokens, "]") || mkerr("expected ] after size"));
    parse!(elemtype = datatype(tokens) || mkerr("expected array element type"));
    succeed(tokens, DataType::Array(size, box elemtype))
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Block> {
    parse!(_ = expect_word(tokens, "{") || NoGo(()));
    let tokens = eatnewlines(tokens);
    parse!(stmts = sep(tokens, stmt, |tokens| {
            parse!(_ = expect_word(tokens, "\n") || NoGo(()));
            succeed(tokens, ())
    }, true) || mkerr("expected statements, you shouldn't see this message"));
    let tokens = eatnewlines(tokens);
    parse!(_ = expect_word(tokens, "}") || mkerr("Block must end with }"));
    succeed(tokens, stmts)
}

fn stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Statement> {
    alt!(return_stmt(tokens), |e| Statement::Return(e));
    alt!(vardef(tokens), |v| Statement::Var(v));
    alt!(expr(tokens), |e| Statement::Expr(e));
    nogo(tokens)
}

fn return_stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect(tokens, |t| t.text == "return") || NoGo(()));
    expr(tokens)
}

// expr = number | ident after_ident
fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    alt!(expect(tokens, |t| t.text.char_at(0).is_numeric()),
            |t:Token<'a>| Expression::Literal(t.text.to_string()));
    alt!(ptr_deref(tokens), |e| Expression::PtrDeref(box e));
    alt!(address_expr(tokens), |e| Expression::Address(box e));
    alt!(array_expr(tokens), |(elems, cont)| Expression::Array(elems, cont));
    alt!(strlit_expr(tokens), |t| Expression::StrLit(t));
    alt_tail!(ident(tokens), |t, id| after_ident(t, Expression::Ident(id)));
    nogo(tokens)
}

fn ptr_deref<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "@") || NoGo(()));
    parse!(e = expr(tokens) || mkerr("expected expression after '@'"));
    succeed(tokens, e)
}

fn address_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "&") || NoGo(()));
    parse!(e = expr(tokens) || mkerr("expected expression after '&'"));
    succeed(tokens, e)
}

fn strlit_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    parse!(t = expect(tokens, |t| t.toktype == StrLit) || NoGo(()));
    let l = t.text.len();
    // todo: auto-concat
    if l >= 2 {
        succeed(tokens, t.text[1..l-1].to_string())
    } else {
        succeed(tokens, String::new())
    }
}

fn array_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, (Vec<Expression>, bool)> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    // comma-separated elements
    parse!(elems = sep(tokens, expr, |tokens| expect_word(tokens, ","), false)
            || mkerr("expected contents in array"));
    // if there is something in the array, can accept ... to indicate
    // that last element
    let (tokens, cont) = {
        if elems.len() > 0 {
            let (tokens, c) = maybeparse!(expect_word(tokens, "..."));
            (tokens, c.is_some())
        } else {
            (tokens, false)
        }
    };
    parse!(_ = expect_word(tokens, "]") || mkerr("array must finish with ']'"));
    succeed(tokens, (elems, cont))
}

// no nogo, optional
fn after_ident<'a>(tokens: Cursor<'a>, id: Expression) -> ParseResult<'a, Token<'a>, Expression, Expression> {
    let id = alt!(equals_tail(tokens, id), |t| t);
    let id = alt!(chainable_follower(tokens, id), |x| x);
    nogo_with(tokens, id)
}

fn chainable_follower<'a>(tokens: Cursor<'a>, base: Expression) -> ParseResult<'a, Token<'a>, Expression, Expression> {
    alt_tail!(funcall_args(tokens),
                |tokens, args| chainable_follower(tokens, Expression::FunCall(box base, args)));
    alt_tail!(subscript_index(tokens),
                |tokens, idx| chainable_follower(tokens, Expression::Subscript(box base, box idx)));
    alt_tail!(dot_syntax(tokens),
                |tokens, mem| chainable_follower(tokens, Expression::Dot(box base, mem)));
    nogo_with(tokens, base)
}

fn equals_tail<'a>(tokens: Cursor<'a>, base: Expression) -> ParseResult<'a, Token<'a>, Expression, Expression> {
    parse!(_ = expect_word(tokens, "=") || NoGo(base));
    parse!(rvalue = expr(tokens) || mkerr("expected expression after '='"));
    // stub
    succeed(tokens, Expression::Assign(box base, box rvalue))
}

fn funcall_args<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Vec<Expression>> {
    parse!(_ = expect_word(tokens, "(") || NoGo(()));
    parse!(args = sep(tokens, expr, |tokens| expect_word(tokens, ","), false)
                    || mkerr("expected args after '('"));
    parse!(_ = expect_word(tokens, ")") || mkerr("Expected ')' after args"));
    succeed(tokens, args)
}

fn subscript_index<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, Expression> {
    parse!(_ = expect_word(tokens, "[") || NoGo(()));
    parse!(e = expr(tokens) || mkerr("expected expression after '['"));
    parse!(_ = expect_word(tokens, "]") || mkerr("Expected ']' after subscript expression"));
    succeed(tokens, e)
}

fn dot_syntax<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Token<'a>, String> {
    parse!(_ = expect_word(tokens, ".") || NoGo(()));
    parse!(s = ident(tokens) || mkerr("expected ident after '.'"));
    succeed(tokens, s)
}

fn expect_word<'a>(tokens: &'a[Token<'a>], expected_text: &str)
    -> ParseResult<'a, Token<'a>, Token<'a>>
{
    expect(tokens, |t| {t.text == expected_text})
}

fn is_ident(t: &Token) -> bool {
    t.text.char_at(0).is_alphabetic()
}

// turns out it's tricky to make this a closure
fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.text == "\n"
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn main() {
    let words = ["...", "->", "%%", "(", ")", "[", "]", "\n",
                "<", ">", "-", "+", "*", "/", "@", "&",
                "!", "$", "{", "}", "%", ",", "=", "#",
                "^", "~", "|", ":", ";", ".", "_", r"\",
                "fun", "or", "and", "var", "return", "break",
                "goto", "ptr"];
    let words: Vec<_> = words.iter().map(|s| LexSpec::Lit(s)).collect();
    // load a file
    let mut programtext = String::new();
    File::open("foo.te").ok().unwrap().read_to_string(&mut programtext).ok().unwrap();
    let specs = [(Whitespace, &[Re(regex!("^[ \t]+"))][..]),
                 (Keyword, &words[..]),
                 (NumLit, &[Re(regex!(r"^[:digit:][xa-fA-F0-9_.]*"))][..]),
                 (StrLit, &[Re(regex!("^\"[^\"]*\""))][..]),
                 (Ident, &[Re(regex!(r"^[\p{Alphabetic}][\p{Alphabetic}\d_]*"))][..]) ];
    let tokens = lexer::lex::<TeToken>(&programtext[..], &specs[..]);
    let tokens: Vec<_> = tokens.filter(|t| t.toktype != Whitespace)
                            .collect();
    let ParseResult(t, parsed) = toplevel(&tokens[..]);
    match parsed {
        Good(tl) => {
            if prettycode::emit_pretty(&mut stdout(), &tl).is_err() {
                println!("failed to generate code, or whatever")
            }
        },
        Error(e) => {
            println!("failed to parse: {:?} at {:?}", e, t[0]);
        },
        NoGo(_) => {
            println!("How does this even happen?");
        }
    }
}
