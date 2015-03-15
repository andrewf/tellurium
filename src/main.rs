#![feature(box_syntax)]
#![feature(collections)]

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

fn is_ident(t: &Token) -> bool {
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

enum AfterIdent {
    Assign(Expression),
    FunCall(Vec<Expression>), // add proto here
    Subscript(Expression)
}

// parse 0 or more Ts, separated by Ss, return in a Vec
fn sep<'a, T, S, FT, FS>(tokens: Cursor<'a>,
                  itemparser: FT,
                  sepparser: FS,
                  canfinishwithsep: bool)
    -> ParseResult<'a, Vec<T>>
    where FT: Fn(Cursor<'a>) -> ParseResult<'a, T>,
          FS: Fn(Cursor<'a>) -> ParseResult<'a, S>
{
    let mut result = Vec::new();
    let mut tokens = match itemparser(tokens) {
        (t, Good(item0)) => {
            result.push(item0);
            t
        }
        (t, NoGo) => {
            return (t, Good(result))  // simply return no items
        }
        (t, Error(e)) => {
            return (t, Error(e))
        }
    };
    // first item is in the vec, go into loop
    loop {
        // try to match separator
        let t = match sepparser(tokens) {
            (t, Good(_)) => {t} // carry on
            (t, NoGo) => {
                // we're done
                return (t, Good(result))
            }
            (t, Error(e)) => { return (t, Error(e)) }
        };
        // got separator, must match item
        tokens = match itemparser(t) {
            (t, Good(it)) => {
                result.push(it);
                t
            }
            (t, NoGo) => {
                if canfinishwithsep {
                    return (t, Good(result))
                } else  {
                    return (t, mkerr("Expected, um, item"))
                }
            }
            (t, Error(e)) => { return (t, Error(e)) }
        }
    }
}



fn ident_expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    parse!(id = ident(tokens) || NoGo);
    let id = Expression::Ident(id);
    let (tokens, after) = maybeparse!(after_ident(tokens));
    let e = match after {
        Some(AfterIdent::Assign(e)) => Expression::Assign(box id, box e),
        Some(AfterIdent::FunCall(args)) => Expression::FunCall(box id, args),
        Some(AfterIdent::Subscript(e)) => Expression::Subscript(box id, box e),
        None => id
    };
    (tokens, Good(e))
}

fn after_ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, AfterIdent> {
    exitif!(assignment_tail(tokens), |t| AfterIdent::Assign(t));
    exitif!(funcall_tail(tokens), |args| AfterIdent::FunCall(args));
    exitif!(subscript_tail(tokens), |sub| AfterIdent::Subscript(sub));
    (tokens, NoGo)
}

fn assignment_tail<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    parse!(_ = expect_word(tokens, "=") || NoGo);
    parse!(e = expr(tokens) || mkerr("Expected expression after '='"));
    (tokens, Good(e))
}

fn funcall_tail<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Vec<Expression>> {
    parse!(_ = expect_word(tokens, "(") || NoGo);
    parse!(args = sep(tokens, expr, |tokens| expect_word(tokens, ","), false)
                    || mkerr("expected args after '('"));
    parse!(_ = expect_word(tokens, ")") || mkerr("Expected ')' after args"));
    (tokens, Good(args))
}

fn subscript_tail<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    parse!(_ = expect_word(tokens, "[") || NoGo);
    parse!(e = expr(tokens) || mkerr("expected expression after '['"));
    parse!(_ = expect_word(tokens, "]") || mkerr("Expected ']' after subscript expression"));
    (tokens, Good(e))
}


// expr = number | ident after_ident
fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    exitif!(expect(tokens, |t| t.text.char_at(0).is_numeric()),
            |t:Token<'a>| Expression::Literal(t.text.to_string()));
    exitif!(ident_expr(tokens), |t| t);
    (tokens, NoGo)
}

fn return_stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    parse!(_ = expect(tokens, |t| t.text == "return") || NoGo);
    expr(tokens)
}

fn stmt<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Statement> {
    exitif!(return_stmt(tokens), |e| Statement::Return(e));
    exitif!(vardef(tokens), |v| Statement::Var(v));
    exitif!(expr(tokens), |e| Statement::Expr(e));
    (tokens, NoGo)
}

fn block<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Block> {
    parse!(_ = expect_word(tokens, "{") || NoGo);
    let tokens = eatnewlines(tokens);
    parse!(stmts = sep(tokens, stmt, |tokens| {
            parse!(_ = expect_word(tokens, "\n") || NoGo);
            (eatnewlines(tokens), Good(()))
    }, true) || mkerr("expected statements, you shouldn't see this message"));
    let tokens = eatnewlines(tokens);
    println!("finishing block with stmts {:?}", stmts);
    parse!(_ = expect_word(tokens, "}") || mkerr("Block must end with }"));
    (tokens, Good(stmts))
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
    //parse!(_ = expect_word(tokens, "\n") || mkerr("expected newline after var"));
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
            let (t, parsed) = toplevel(&tokens[..]);
            //println!("{:?}", toplevel(&tokens[..]));
            match parsed {
                Good(tl) => {
                    if prettycode::emit_pretty(&mut stdout(), &tl).is_err() {
                        println!("failed to generate code, or whatever")
                    }
                },
                Error(e) => {
                    println!("failed to parse: {:?} at {:?}", e, t[0]);
                },
                NoGo => {
                    println!("How does this even happen?");
                }
            }
        }
        Err(tok) => {
            println!("Lexer error. unexpected token {:?}", tok)
        }
    }
}
