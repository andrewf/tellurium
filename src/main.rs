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
use recdec::ParseStatus::*;
use parsetree::*;

// single-token parsers should only return NoGo, shouldn't use
// expect

fn is_ident<'a>(t: &Token<'a>) -> bool {
    t.text.char_at(0).is_alphabetic()
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, String> {
    match expect(tokens, is_ident) {
        (t, Good(tok)) => {
            (t, Good(tok.text.to_string()))
        }
        (t, _) => {
            (t, NoGo)
        }
    }
}

// expr = number | ident after_ident
fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    exitif!(expect(tokens, |t| t.text.char_at(0).is_numeric()),
            |t:Token<'a>| Expression::Literal(t.text.to_string()));
    match ident(tokens) {
        (tokens, Good(id)) => {
            // maybe parens?
            if peek_pred(tokens, &|t| t.text == "(") {
                //println!("fncall");
                let mut args = Vec::new();
                let mut tokens = &tokens[1..]; // move on
                while !peek_pred(tokens, &|t| t.text == ")") {
                tokens = {
                        // argument
                        parse!(arg = expr(tokens) || mkerr("Expected expression as argument"));
                        //println!(" arg: {:?}", arg);
                        args.push(arg);
                        if peek_pred(tokens, &|t| t.text == ",") {
                            //println!("  next arg");
                            ignore(tokens, |t| t.text == ",")
                        } else if peek_pred(tokens, &|t| t.text == ")") {
                            tokens // don't change anything else
                        } else {
                            return (tokens, mkerr("Exected , or ) after argument"))
                        }
                    }
                }
                //println!("arg loop finished");
                parse!(_ = expect(tokens, |t| t.text == ")") || mkerr("expected ) after args"));
                //println!("no more args");
                return (tokens, Good(Expression::FnCall(id, args)))
            } else {
                // just a name
                (tokens, Good(Expression::Ident(id)))
            }
        }
        (tokens, _) => {
            (tokens, Error(ParseError::new("expected expression")))
        }
    }
}

fn return_stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    parse!(_ = expect(tokens, |t| t.text == "return") || NoGo);
    expr(tokens)
}

fn stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Statement> {
    exitif!(return_stmt(tokens), |e| Statement::Return(e));
    exitif!(vardef(tokens), |v| Statement::Var(v));
    exitif!(expr(tokens), |e| Statement::Expr(e));
    (tokens, Error(ParseError::new("Expected statement (vardef or expression")))
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Block> {
    parse!(_ = expect_word(tokens, "{") || mkerr("Block must start with {"));
    let mut ret = Vec::new();
    let mut tokens = eatnewlines(tokens);
    while tokens.len() > 0 && !peek_pred(tokens, &|t| t.text == "}") {
        let t = tokens; // alias for macro
        //println!("looking for body expr");
        parse!(s = stmt(t) || mkerr("Expected statement"));
        //println!("  got expr {:?}", e);
        ret.push(s);
        tokens = eatnewlines(t);
    }
    parse!(_ = expect_word(tokens, "}") || mkerr("Block must end with }"));
    (tokens, Good(ret))
}

fn datatype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, DataType> {
    if peek_pred(tokens, &|t| { t.text == "ptr" }) {
        // ptr to ...
        parse!(_ = expect_word(tokens, "ptr") || mkerr("I just checked this, seriously"));
        parse!(referrent = datatype(tokens) || mkerr("'ptr' must be followed by datatype"));
        (tokens, Good(DataType::Pointer(box referrent)))
    } else if peek_pred(tokens, &|t| { t.text == "["}) {
        // array of...
        parse!(_ = expect_word(tokens, "[") || mkerr("just checked for opening bracket"));
        parse!(size = expr(tokens) || mkerr("expected size expression"));
        parse!(_ = expect_word(tokens, "]") || mkerr("Expected ] after array size"));
        parse!(referrent = datatype(tokens) || mkerr("expected array element type"));
        (tokens, Good(DataType::Array(size, box referrent)))
    } else {
        // just a name
        parse!(name = ident(tokens) || mkerr("expected data type"));
        (tokens, Good(DataType::Named(name)))
    }
}

fn parse_arglist<'a>(tokens: Cursor<'a>) -> ParseResult<'a, ArgList> {
    let mut ret = Vec::new();
    parse!(_ = expect(tokens, |t| t.text == "(") || NoGo);
    let mut tokens = tokens;
    //let (mut tokens, _) = try!(expect_word(tokens, "expected ( at start of params", "("));
    while !peek_pred(tokens, &|t| {t.text ==  ")"}) {
        let t = tokens; // rename it so macro can redefine it, while keeping mut version
        parse!(name = ident(t) || mkerr("expected param name"));
        parse!(dt = datatype(t) || mkerr("expected param type")); // todo parse types
        ret.push((name, dt)); // store them in list
        // break if no comma
        match expect_word(t, ",") {
            (t_after_comma, Good(_)) => {
                tokens = t_after_comma
            }
            _ => {
                tokens = t;
                break;
            }
        }
    }
    parse!(_ = expect_word(tokens, ")") || mkerr("expected ) after params"));
    (tokens, Good(ret))
}

fn fundef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, FunDef> {
    parse!(_  = expect_word(tokens, "fun") || NoGo);
    parse!(name = ident(tokens) || mkerr("expected function name"));
    parse!(args = parse_arglist(tokens) || mkerr("expected arglist"));
    // maybe return type
    let (tokens, ret_type) =
        match expect(tokens, |t| { t.text == "->" }) {
            (tokens, Good(_)) => {
                parse!(t = datatype(tokens) || mkerr("expected type after ->"));
                (tokens, t)
            }
            _ => (tokens, DataType::Void)
        };
    // body
    parse!(body = block(tokens) || mkerr("expected function body"));
    (tokens, Good(FunDef::new(name, args, ret_type, body)))
}

fn vardef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, VarDef> {
    parse!(_ = expect_word(tokens, "var") || NoGo);
    parse!(name = ident(tokens) || mkerr("expected variable name"));
    parse!(t = datatype(tokens) || mkerr("expected variable type"));
    parse!(_ = expect_word(tokens, "=") || mkerr("variable must be initialized with ="));
    parse!(e = expr(tokens) || mkerr("expected initialization expression after ="));
    parse!(_ = expect_word(tokens, "\n") || mkerr("expected newline after var"));
    (tokens, Good(VarDef::new(name, t, e)))
}

// turns out it's tricky to make this a closure
fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.text == "\n"
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn toplevelitem<'a>(tokens: Cursor<'a>) -> ParseResult<'a, TopLevelItem> {
    exitif!(vardef(tokens), |v| TopLevelItem::VarDef(v));
    exitif!(fundef(tokens), |f| TopLevelItem::FunDef(f));
    (tokens, Error(ParseError::new("expected fun def or var def at top level")))
}

fn toplevel<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, TopLevel> {
    let mut tl = TopLevel::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        let newtok = tokens;
        //let (newtok, item) = try!(toplevelitem(tokens));
        parse!(item = toplevelitem(newtok) || mkerr("expected fun or var def"));
        tokens = newtok;
        tl.push(item);
        tokens = eatnewlines(tokens);
    }
    (tokens, Good(tl))
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
            let (_, parsed) = toplevel(&tokens[..]);
            //println!("{:?}", toplevel(&tokens[..]));
            match parsed {
                Good(tl) => {
                    if prettycode::emit_pretty(&mut stdout(), &tl).is_err() {
                        println!("failed to generate code, or whatever")
                    }
                },
                _ => {}
            }
        }
        Err(tok) => {
            println!("Lexer error. unexpected token {:?}", tok)
        }
    }
}
