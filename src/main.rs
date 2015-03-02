#![feature(box_syntax)]

use std::char::CharExt;

#[derive(Clone, Debug, PartialEq)]
enum TokenType {
    Word,  // includes keywords, numbers
    //Plus,
    //Minus,
    //Mult,
    //Divide,
    AddressOp,
    DerefOp,
    ProtocolOp,
    GenericTrigger,
    Comma,
    Newline,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
    ParenOpen,
    ParenClose,
    LessThan,
    GreaterThan
}

use TokenType::*;

#[derive(Debug, Clone)]
struct Token<'a> {
    toktype: TokenType,
    line: usize,
    column: usize,
    text: &'a str
}

fn lex<'a>(input: &'a str) -> Vec<Token<'a>> {
    let mut begin = 0;
    let mut end = 0;
    let mut t: Option<TokenType> = None; // None means we're between tokens
    let mut ret = Vec::new();
    let mut line = 1;
    let mut col = 1;
    for (i, c) in input.chars().enumerate() {
        let oldt = t;
        end = i;
        // want to specify changes to column in loop, apply later
        let mut column_delta = 0;
        // most of these are single-char tokens, so we will want
        // to put them in the token vec right away
        let mut flush = true;
        // update tokenizer state
        t = if c == '(' {
            Some(ParenOpen)
        } else if c == ')' {
            Some(ParenClose)
        } else if c == '\n' {
            Some(Newline)
        } else if c == '{' {
            Some(CurlyOpen)
        } else if c == '}' {
            Some(CurlyClose)
        } else if c == '[' {
            Some(SquareOpen)
        } else if c == ']' {
            Some(SquareClose)
        } else if c == '&' {
            Some(AddressOp)
        } else if c == '@' {
            Some(DerefOp)
        } else if c == '<' {
            Some(LessThan)
        } else if c == '>' {
            Some(GreaterThan)
        } else if c == '|' {
            Some(ProtocolOp)
        } else if c == '!' {
            Some(GenericTrigger)
        } else if c == ',' {
            Some(Comma)
        } else if c.is_whitespace() {
            column_delta += 1;
            None
        } else {
            flush = false;
            Some(Word)
        };
        // if we're transitioning to a new type of token,
        // then we definitely want to flush the current one.
        if !(t == oldt) {
            flush = true
        };
        // check if we need to add a new token object
        if flush {
            if let Some(prevt) = oldt {
                // flush previous token
                ret.push(Token{
                    toktype: prevt,
                    line: line,
                    column: col,
                    text: &input[begin..end]
                });
                column_delta += end - begin;
            }
            // reset indices for current token
            // of which there must be only one char so far
            begin = i;
            end = i;
        }
        // move line and column counters
        col += column_delta;
        if c == '\n' {
            line += 1;
            // this makes the newline token be column 0, first char is 1
            col = 0;
        }
    }
    // flush last token, if any
    if let Some(lastt) = t {
        // note: end hasn't been advanced past-end yet, so we have
        // to do that manually
        ret.push(Token{
            toktype: lastt,
            line: line,
            column: col,
            text: &input[begin..end+1]
        })
    }
    ret
}

#[derive(Debug)]
struct ParseError {
    msg: String,
    line: usize,
    column: usize
}

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

#[derive(Debug)]
struct FunDef {
    ld_name: String,
    //convention: Option<String>,
    //polymorphic_name: Option<String>,
    args: ArgList,
    return_type: DataType,
    //body: Block
}

//fn make_fundef(n: String, a: ArgList

type Cursor<'a> = &'a[Token<'a>];
type ParseResult<'a, T> = Result<(Cursor<'a>, T), ParseError>;


fn expect<'a, F: Fn(&Token<'a>)->bool, S: ToString >(tokens: Cursor<'a>, msg: S, f: F)
                    -> ParseResult<'a, Token<'a>>
{
    // either returns rest or a ParseError
    if tokens.len() > 0 {
        let ref front = tokens[0];
        if f(front) {
            Ok((&tokens[1..], (*front).clone()))
        } else {
            Err(ParseError{
                    msg: msg.to_string(),
                    line: front.line,
                    column: front.column
            })
        }
    } else {
        Err(ParseError{msg: "Unexpected end of input".to_string(), line: 0, column: 0})
    }
}

fn expect_type<'a, S: ToString>(tokens: Cursor<'a>, msg: S, expected_type: TokenType)
                -> ParseResult<'a, Token<'a>>
{
    expect(tokens, msg, |t| { t.toktype == expected_type })
}

fn expect_word<'a, S: ToString>(tokens: Cursor<'a>, msg: S, expected_text: &str)
                -> ParseResult<'a, Token<'a>>
{
    // to_string here is a kludge to avoid moving msg so we can use it later
    let (tokens, tok) = try!(expect_type(tokens, msg.to_string(), TokenType::Word));
    if tok.text != expected_text {
        Err(ParseError{msg: msg.to_string(), line: tok.line, column: tok.column})
    } else {
        Ok((tokens, tok))
    }
}

fn peek_pred<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: &F) -> bool {
    tokens.len() > 0 && f(&tokens[0])
}

// if the next token matches f, pop it off, otherwise do nothing
// someday, this will be useful
//fn ignore<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: F) -> Cursor<'a> {
//    if tokens.len() > 0 && f(&tokens[0]) {
//        &tokens[1..]
//    } else {
//        tokens
//    }
//}

fn ignore_many<'a, F: Fn(&Token<'a>)->bool>(mut tokens: Cursor<'a>, f: F) -> Cursor<'a> {
    while peek_pred(tokens, &f) {
        tokens = &tokens[1..];
    }
    tokens
}

fn ident<'a>(tokens: Cursor<'a>) -> ParseResult<'a, String> {
    let (cur, tok) = try!(expect_type(tokens, "Expected identifier", TokenType::Word));
    // later, maybe check it's not a reserved word
    Ok((cur, tok.text.to_string()))
}

fn expr<'a>(tokens: Cursor<'a>) -> ParseResult<'a, Expression> {
    let (tokens, tok) = try!(expect_type(tokens, "Expected expression", TokenType::Word));
    Ok((tokens, tok.text.to_string()))
}

fn datatype<'a>(tokens: Cursor<'a>) -> ParseResult<'a, DataType> {
    if peek_pred(tokens, &|t| { t.text == "ptr" }) {
        // ptr to ...
        let (tokens, _) = try!(expect_word(tokens, "I just checked this, seriously", "ptr"));
        let (tokens, referrent) = try!(datatype(tokens));
        Ok((tokens, DataType::Pointer(box referrent)))
    } else if peek_pred(tokens, &|t| { t.toktype == TokenType::SquareOpen }) {
        // array of...
        let (tokens, _) = try!(expect_type(tokens, "just checked for opening bracket", TokenType::SquareOpen));
        let (tokens, size) = try!(expr(tokens));
        let (tokens, _) = try!(expect_type(tokens, "Expected ] after array size", TokenType::SquareClose));
        let (tokens, referrent) = try!(datatype(tokens));
        Ok((tokens, DataType::Array(size, box referrent)))
    } else {
        // just a name
        let (tokens, name) = try!(ident(tokens));
        Ok((tokens, DataType::Named(name)))
    }
}

fn parse_arglist<'a>(tokens: Cursor<'a>) -> ParseResult<'a, ArgList> {
    let mut ret = Vec::new();
    let (mut tokens, _) = try!(expect_type(tokens, "expected ( at start of params", TokenType::ParenOpen));
    while !peek_pred(tokens, &|t| {t.toktype ==  TokenType::ParenClose}) {
        let (t, name) = try!(ident(tokens));
        let (t, dt) = try!(datatype(t)); // todo parse types
        ret.push((name, dt)); // store them in list
        // break if no comma
        match expect_type(t, "expected , between params", TokenType::Comma) {
            Ok((t_after_comma, _)) => {
                tokens = t_after_comma
            }
            Err(_) => {
                tokens = t;
                break;
            }
        }
    }
    let (t, _) = try!(expect_type(tokens, "expected ) after params", TokenType::ParenClose));
    Ok((t, ret))
}
    

fn fundef<'a>(tokens: Cursor<'a>) -> ParseResult<'a, FunDef> {
    let (tokens, _)  = try!(expect_word(tokens, "um", "fun"));
    let (tokens, name) = try!(ident(tokens));
    let (tokens, args) = try!(parse_arglist(tokens));
    // maybe return type
    println!("next tok {:?}", tokens[0]);
    let (tokens, ret_type) =
        match expect(tokens, "", |t| { t.text == "to" }) {
            Ok((zzz, _)) => {
                println!("saw arrow, looking for return type");
                try!(datatype(zzz))
            }
            _ => (tokens, DataType::Void)
        };
    // body
    let (tokens, _) = try!(expect_type(tokens, "expected {", TokenType::CurlyOpen));
    let (tokens, _) = try!(expect_type(tokens, "expected }", TokenType::CurlyClose));
    Ok((tokens,
        FunDef{
            ld_name: name,
            args: args,
            return_type: ret_type
    }))
}

fn isnewline<'a>(t: &Token<'a>) -> bool {
    t.toktype == TokenType::Newline
}

fn eatnewlines<'a>(tokens: Cursor<'a>) -> Cursor<'a> {
    ignore_many(tokens, isnewline)
}

fn fundefs<'a>(mut tokens: Cursor<'a>) -> ParseResult<'a, Vec<FunDef>> {
    let mut ret = Vec::new();
    // eat newlines
    tokens = eatnewlines(tokens);
    while tokens.len() > 0 {
        let (newtok, s) = try!(fundef(tokens));
        tokens = newtok;
        tokens = eatnewlines(tokens);
        ret.push(s);
    }
    Ok((tokens, ret))
}

fn main() {
    let data = "foo( )([ (\n4[) ) urk> <%% @ !6& ptr $";
    for t in lex(&data).iter() {
        println!(
            "{:?} ({}:{}): {:?}",
            t.toktype, t.line, t.column, t.text
        )
    }
    println!("{:?}", fundefs(&lex(&"\n\n  \n  fun grup ( a [3]b, c ptr [5] d,) {} \n\n fun   foo () to [r]i32 {}\n")[..]))
}
