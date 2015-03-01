use std::char::CharExt;

#[derive(Debug, PartialEq)]
enum TokenType {
    Word,  // includes keywords
    //Plus,
    //Minus,
    //Mult,
    //Divide,
    AddressOp,
    DerefOp,
    ProtocolOp,
    GenericTrigger,
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
    
struct Token<'a> {
    toktype: TokenType,
    line: usize,
    column: usize,
    bytes: &'a[u8]
}

fn lex<'a>(bytes: &'a Vec<u8>) -> Vec<Token<'a>> {
    let mut begin = 0;
    let mut end = 0;
    let mut t: Option<TokenType> = None; // None means we're between tokens
    let mut ret = Vec::new();
    let mut line = 1;
    let mut col = 1;
    for (i, c) in bytes.iter().enumerate() {
        let oldt = t;
        end = i;
        // want to specify changes to column in loop, apply later
        let mut column_delta = 0;
        // most of these are single-char tokens, so we will want
        // to put them in the token vec right away
        let mut flush = true;
        let c = *c as char;
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
                    bytes: &bytes[begin..end]
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
            bytes: &bytes[begin..end+1]
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

enum DataType {
    Pointer(Box<DataType>),
    Array(u64, Box<DataType>),
    Tuple(Vec<DataType>),
    Named(GenericName) // plain strings go here
}                      // we can tell from context that it's a type

type ArgList = Vec<(String, DataType)>;

struct FunDef {
    ld_name: String,
    convention: Option<String>,
    polymorphic_name: Option<String>,
    args: ArgList,
    return_type: DataType,
    body: Block
}
*/

fn expect<'a, F: Fn(&Token<'a>)->bool, S: ToString >(tokens: &'a[Token<'a>], msg: S, f: F)
                    -> Result<&'a[Token<'a>], ParseError> {
    // either returns rest or a ParseError
    if tokens.len() > 0 {
        let ref front = tokens[0];
        if f(front) {
            Ok(&tokens[1..])
        } else {
            Err(ParseError{
                    msg: msg.to_string(),
                    line: front.line,
                    column: front.column
            })
        }
    } else {
        Err(ParseError{msg: String::from_str("Unexpected end of input"), line: 0, column: 0})
    }
}

fn nesty<'a>(mut tokens: &'a[Token<'a>]) -> Result<(&'a[Token<'a>], i32), ParseError> {
    // nesty -> '(' nesty ')' | '(' ')'
    // unconditional (
    tokens = try!(expect(tokens, "Expected '('", |t| {t.toktype == TokenType::ParenOpen}));
    // recurse if possible
    let inner = match nesty(tokens) {
        Ok((tok, n)) => {tokens = tok; n}
        Err(_) => {println!("base case"); 0 } // just means couldn't match '(', base case
    };
    // close paren
    tokens = try!(expect(tokens, "Expected ')'", |t| {t.toktype == TokenType::ParenClose}));
    // add 1 to count, to represent this call
    Ok((tokens, inner + 1))
}

fn toplevel<'a>(mut tokens: &'a[Token<'a>]) -> Result<Vec<i32>, ParseError> {
    // parse a sequence of function definitions
    let mut ret = Vec::new();
    while tokens.len() > 0 {
        match nesty(tokens) {
            Ok((newtok, n)) => {
                tokens = newtok;
                ret.push(n)
            }
            Err(e) => return Err(e)
        }
    }
    Ok(ret)
}


fn main() {
    //let data = String::from_str("foo( )([ (\n4[) ) urk> <%% @ !6& ptr $").into_bytes();
    //for t in lex(&data).iter() {
    //    println!(
    //        "{:?} ({}:{}): {:?}",
    //        t.toktype, t.line, t.column,
    //        String::from_utf8_lossy(t.bytes)
    //    )
    //}
    let data = String::from_str("(())()()()").into_bytes();
    for t in lex(&data).iter() {
        println!(
            "{:?} ({}:{}): {:?}",
            t.toktype, t.line, t.column,
            String::from_utf8_lossy(t.bytes)
        )
    }
    println!("{:?}", toplevel(&lex(&String::from_str("((()))()(((((())))))(())").into_bytes())[..]))
}
