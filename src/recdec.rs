
use lexer::Token;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
}

impl ParseError {
    pub fn new<S: ToString>(msg: S) -> ParseError {
        ParseError {
            msg: msg.to_string()
        }
    }
}

pub enum ParseStatus<T> {
    Good(T),
    NoGo,   // amicable failure, no commitment
    Error(ParseError)  // parse failed, forever
}

pub type Cursor<'a> = &'a[Token<'a>];
pub type ParseResult<'a, T> = (Cursor<'a>, ParseStatus<T>);

pub fn expect<'a, F: Fn(&Token<'a>)->bool, S: ToString >(tokens: Cursor<'a>, msg: S, f: F)
                    -> ParseResult<'a, Token<'a>>
{
    // either returns rest or a ParseError
    if tokens.len() > 0 {
        let ref front = tokens[0];
        if f(front) {
            (&tokens[1..], ParseStatus::Good((*front).clone()))
        } else {
            (tokens, ParseStatus::Error(ParseError::new(msg.to_string())))
        }
    } else {
        (tokens, ParseStatus::Error(ParseError::new("Unexpected end of input")))
    }
}

// like expect, but returns NoGo instead of error
pub fn hopefor<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: F)
                    -> ParseResult<'a, Token<'a>>
{
    if tokens.len() > 0 {
        let ref front = tokens[0];
        if f(front) {
            (&tokens[1..], ParseStatus::Good((*front).clone()))
        } else {
            (tokens, ParseStatus::NoGo)
        }
    } else {
        (tokens, ParseStatus::NoGo)
    }
}

pub fn expect_word<'a, S: ToString>(tokens: Cursor<'a>, msg: S, expected_text: &str)
                -> ParseResult<'a, Token<'a>>
{
    expect(tokens, msg, |t| {t.text == expected_text})
}

pub fn peek_pred<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: &F) -> bool {
    tokens.len() > 0 && f(&tokens[0])
}

// if the next token matches f, pop it off, otherwise do nothing
pub fn ignore<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: F) -> Cursor<'a> {
    if tokens.len() > 0 && f(&tokens[0]) {
        &tokens[1..]
    } else {
        tokens
    }
}

// skip stuff in the stream. Can't fail, so doesn't return Result,
// just advances cursor
pub fn ignore_many<'a, F: Fn(&Token<'a>)->bool>(mut tokens: Cursor<'a>, f: F) -> Cursor<'a> {
    while peek_pred(tokens, &f) {
        tokens = &tokens[1..];
    }
    tokens
}

// demand success of parsing function, no form of failure allowed
// NoGo means the caller still fails
#[macro_export]
macro_rules! parse {
    // here, token list is only argument
    // let (tokens, r) = try!(f(tokens))
    ( $r:pat = $f:ident ( $token_ident:ident ) ) => {
        let ($token_ident, $r) =  {
            let (t, st) = $f($token_ident); 
            match st {
                $crate::recdec::ParseStatus::Good(item) => (t, item),
                _ => return (t, $crate::recdec::ParseStatus::Error(ParseError::new("uh crap")))
            }
        }
    };
    // also other args to parse function
    // let (tokens, r) = try!(f(tokens, args...))
    ( $r:pat = $f:ident ( $token_ident:ident, $($a:expr),* ) ) => {
        let ($token_ident, $r) = {
            let (t, st) = $f($token_ident, $($a),* );
            match st {
                $crate::recdec::ParseStatus::Good(item) => (t, item),
                _ => return (t, $crate::recdec::ParseStatus::Error(ParseError::new("uh crap")))
            }
        }
    }
}

// macro to return ok result from function a call succeeds
// useful for parsing alternatives. Also returns Error if it
// gets an error, since that indicates an unrecoverable problem.
#[macro_export]
macro_rules! exitif {
    ($expr:expr, $f:expr) => (match $expr {
        (t, $crate::recdec::ParseStatus::Good(val)) => return (t, ParseStatus::Good($f(val))),
        (t, $crate::recdec::ParseStatus::Error(e)) => return (t, ParseStatus::Error(e)),
        (t, $crate::recdec::ParseStatus::NoGo) => {}
    })
}


// like parse!, but returns NoGo instead of error
#[macro_export]
macro_rules! nogoifnot {
    ($r:pat = $f:ident ( $token_ident:ident, $($a:expr),* ) ) => {
        let ($token_ident, $r) = {
            let (t, st) = $f($token_ident, $($a),* );
            match st {
                ParseStatus::Good(item) => (t, item),
                _ => return ($token_ident, ParseStatus::NoGo)
            }
        }
    };
    ($r:pat = $f:ident ( $token_ident:ident, $($a:expr),* ) ) => {
        let ($token_ident, $r) = {
            let (t, st) = $f($token_ident, $($a),* );
            match st {
                ParseStatus::Good(item) => (t, item),
                _ => return ($token_ident, ParseStatus::NoGo)
            }
        }
    }
}
