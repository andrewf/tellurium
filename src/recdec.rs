
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

pub fn mkerr<T, S: ToString>(msg: S) -> ParseStatus<T> {
    ParseStatus::Error(ParseError::new(msg))
}

pub type Cursor<'a> = &'a[Token<'a>];

pub struct ParseResult<'a, T>(pub Cursor<'a>, pub ParseStatus<T>);

pub fn succeed<'a, T>(tokens: Cursor<'a>, it: T ) -> ParseResult<'a, T> {
    ParseResult(tokens, ParseStatus::Good(it))
}
pub fn nogo<'a, T>(tokens: Cursor<'a> ) -> ParseResult<'a, T> {
    ParseResult(tokens, ParseStatus::NoGo)
}
pub fn parsefail<'a, T, S: ToString>(tokens: Cursor<'a>, msg: S) -> ParseResult<'a, T> {
    ParseResult(tokens, mkerr(msg))
}

impl<'a, T> ParseResult<'a, T> {
    pub fn fmap<U, F>(self, f: F) -> ParseResult<'a, U> where F: FnOnce(T)->U {
        match self {
            ParseResult(t, ParseStatus::Good(it)) => ParseResult(t, ParseStatus::Good(f(it))),
            ParseResult(t, ParseStatus::NoGo) => ParseResult(t, ParseStatus::NoGo),
            ParseResult(t, ParseStatus::Error(e)) => ParseResult(t, ParseStatus::Error(e))
        }
    }
}

pub fn expect<'a, F: Fn(&Token<'a>)->bool >(tokens: Cursor<'a>, f: F)
                    -> ParseResult<'a, Token<'a>>
{
    // either returns rest or a ParseError
    if tokens.len() > 0 {
        let ref front = tokens[0];
        if f(front) {
            succeed(&tokens[1..], (*front).clone())
        } else {
            nogo(tokens)
        }
    } else {
        nogo(tokens)
    }
}

pub fn expect_word<'a>(tokens: Cursor<'a>, expected_text: &str)
                -> ParseResult<'a, Token<'a>>
{
    expect(tokens, |t| {t.text == expected_text})
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

// parse 0 or more Ts, separated by Ss, return in a Vec
pub fn sep<'a, T, S, FT, FS>(tokens: Cursor<'a>,
                  itemparser: FT,
                  sepparser: FS,
                  canfinishwithsep: bool)
    -> ParseResult<'a, Vec<T>>
    where FT: Fn(Cursor<'a>) -> ParseResult<'a, T>,
          FS: Fn(Cursor<'a>) -> ParseResult<'a, S>
{
    let mut result = Vec::new();
    let mut tokens = match itemparser(tokens) {
        ParseResult(t, ParseStatus::Good(item0)) => {
            result.push(item0);
            t
        }
        ParseResult(_, ParseStatus::NoGo) => {
            return succeed(tokens, result)  // simply return no items
        }
        ParseResult(t, ParseStatus::Error(e)) => {
            return ParseResult(t, ParseStatus::Error(e))
        }
    };
    // first item is in the vec, go into loop
    loop {
        // try to match separator
        let t = match sepparser(tokens) {
            ParseResult(t, ParseStatus::Good(_)) => {t} // carry on
            ParseResult(_, ParseStatus::NoGo) => {
                // we're done
                return succeed(tokens, result)
            }
            ParseResult(t, ParseStatus::Error(e)) => {
                return ParseResult(t, ParseStatus::Error(e))
            }
        };
        // got separator, must match item
        tokens = match itemparser(t) {
            ParseResult(t, ParseStatus::Good(it)) => {
                result.push(it);
                t
            }
            ParseResult(t, ParseStatus::NoGo) => {
                if canfinishwithsep {
                    return succeed(tokens, result)
                } else  {
                    return parsefail(t, "Expected, um, item")
                }
            }
            ParseResult(t, ParseStatus::Error(e)) => {
                return ParseResult(t, ParseStatus::Error(e))
            }
        }
    }
}

// try! adapted for ParseResult
// passes through successful results, fails on NoGo, and
// propagates existing errors.
macro_rules! parse_try {
    ($parseresult:expr, $errval:expr) => {
        match $parseresult {
            ParseResult(t, $crate::recdec::ParseStatus::Good(item)) => (t, item),
            ParseResult(t, $crate::recdec::ParseStatus::NoGo) => return ParseResult(t, $errval),
            ParseResult(t, $crate::recdec::ParseStatus::Error(e)) => return ParseResult(t, Error(e))
        }
    };
}


// demand success of parsing function, no form of failure allowed
// NoGo means the caller still fails
#[macro_export]
macro_rules! parse {
    // here, token list is only argument
    // let (tokens, r) = try!(f(tokens))
    ( $r:pat = $f:ident ( $token_ident:ident ) || $errval:expr ) => {
        let ($token_ident, $r) = parse_try!( $f($token_ident), $errval );
    };
    // like above, but between >> and || is a function through which the parsed
    // value is filtered
//    ( $r:pat= $f:ident ( $token_ident:ident) >> $after:expr || $errval:expr ) => {
//        let ($token_ident, $r) = {
//            let (t, r) = parse_try!( $f($token_ident) );
//            (t, $after(r))
//        }
//    }
    // also other args to parse function
    // let (tokens, r) = try!(f(tokens, args...))
    ( $r:pat = $f:ident ( $token_ident:ident, $($a:expr),* ) || $errval:expr ) => {
        let ($token_ident, $r) = parse_try!( $f($token_ident, $($a),*), $errval );
    }
}

// macro to return ok result from function a call succeeds
// useful for parsing alternatives. Also returns Error if it
// gets an error, since that indicates an unrecoverable problem.
// nogo means go on to the next option
#[macro_export]
macro_rules! alt {
    ($expr:expr, $f:expr) => (match $expr {
        ParseResult(t, $crate::recdec::ParseStatus::Good(val)) =>
            return ParseResult(t, ParseStatus::Good($f(val))),
        ParseResult(t, $crate::recdec::ParseStatus::Error(e)) =>
            return ParseResult(t, ParseStatus::Error(e)),
        ParseResult(_, $crate::recdec::ParseStatus::NoGo) => {}
    })
}

// like alt, except the passed function is more like a
// parser, in that it's tokens->...->ParseResult, which is useful for
// left-associative rules
// this is a terrible name
#[macro_export]
macro_rules! alt_tail {
    ($parsed:expr, $tailparser:expr) => {
        match $parsed {
            ParseResult(t, ParseStatus::Good(val)) =>
                return $tailparser(t, val),
            ParseResult(t, ParseStatus::Error(e)) =>
                return ParseResult(t, ParseStatus::Error(e)),
            ParseResult(_, ParseStatus::NoGo) =>{}
        }
    }
}
                
            


// returns an option<T> depending on whether the result of the parse
// is Good or NoGo. if Error, returns error.
#[macro_export]
macro_rules! maybeparse {
    ($parsecall:expr) => (
        match $parsecall {
            ParseResult(t, $crate::recdec::ParseStatus::Good(val)) => (t, Some(val)),
            ParseResult(t, $crate::recdec::ParseStatus::NoGo) => (t, None),
            ParseResult(t, $crate::recdec::ParseStatus::Error(e)) => return ParseResult(t, Error(e))
        }
     )
}
