use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
}

impl ParseError {
    pub fn new<S: ToString>(msg: S) -> ParseError {
        ParseError { msg: msg.to_string() }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.msg)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        &self.msg
    }
    fn cause(&self) -> Option<&Error> {
        None
    }
}



pub enum ParseStatus<T, N = ()> {
    // successful parse
    Good(T),
    // amicable failure, no commitment, possibly
    // "un-moving" data passed into fn
    NoGo(N),
    // parse failed, forever
    Fail(ParseError),
}

pub fn mkfail<T, N, S: ToString>(msg: S) -> ParseStatus<T, N> {
    ParseStatus::Fail(ParseError::new(msg))
}

pub struct ParseResult<'a, Tok, T, N = ()>(
                                           pub &'a [Tok],
                                           pub ParseStatus<T, N>)
    where Tok: 'a;

pub fn succeed<'a, Tok, T, N>(tokens: &'a [Tok], it: T) -> ParseResult<'a, Tok, T, N> {
    ParseResult(tokens, ParseStatus::Good(it))
}
pub fn nogo<'a, Tok, T>(tokens: &'a [Tok]) -> ParseResult<'a, Tok, T> {
    ParseResult(tokens, ParseStatus::NoGo(()))
}
pub fn nogo_with<'a, Tok, T, N>(tokens: &'a [Tok], n: N) -> ParseResult<'a, Tok, T, N> {
    ParseResult(tokens, ParseStatus::NoGo(n))
}
pub fn parsefail<'a, Tok, T, N, S: ToString>(tokens: &'a [Tok],
                                             msg: S)
                                             -> ParseResult<'a, Tok, T, N> {
    ParseResult(tokens, mkfail(msg))
}

impl<'a, Tok, T> ParseResult<'a, Tok, T, T> {
    pub fn nogo_is_good<N>(self) -> ParseResult<'a, Tok, T, N> {
        // pass through, but turn NoGo into Good
        match self {
            ParseResult(t, ParseStatus::Good(it)) => ParseResult(t, ParseStatus::Good(it)),
            ParseResult(t, ParseStatus::NoGo(it)) => ParseResult(t, ParseStatus::Good(it)),
            ParseResult(t, ParseStatus::Fail(e)) => ParseResult(t, ParseStatus::Fail(e)),
        }
    }
}

pub fn expect<'a, Tok: Clone, F: Fn(&Tok) -> bool>(tokens: &'a [Tok],
                                                   f: F)
                                                   -> ParseResult<'a, Tok, Tok> {
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

pub fn peek_pred<'a, Tok, F: Fn(&Tok) -> bool>(tokens: &'a [Tok], f: &F) -> bool {
    tokens.len() > 0 && f(&tokens[0])
}

// if the next token matches f, pop it off, otherwise do nothing
// pub fn ignore<'a, Tok, F: Fn(&Tok)->bool>(tokens: &'a[Tok], f: F) -> &'a[Tok] {
//    if tokens.len() > 0 && f(&tokens[0]) {
//        &tokens[1..]
//    } else {
//        tokens
//    }
// }

// skip stuff in the stream. Can't fail, so doesn't return Result,
// just advances cursor
pub fn ignore_many<'a, Tok, F: Fn(&Tok) -> bool>(mut tokens: &'a [Tok], f: F) -> &'a [Tok] {
    while peek_pred(tokens, &f) {
        tokens = &tokens[1..];
    }
    tokens
}

// parse 0 or more Ts, separated by Ss, return in a Vec
pub fn sep<'a, Tok: Clone, T, S, FT, FS>(tokens: &'a [Tok],
                                         itemparser: FT,
                                         sepparser: FS,
                                         canfinishwithsep: bool)
                                         -> ParseResult<'a, Tok, Vec<T>>
    where FT: Fn(&'a [Tok]) -> ParseResult<'a, Tok, T>,
          FS: Fn(&'a [Tok]) -> ParseResult<'a, Tok, S>
{
    let mut result = Vec::new();
    let mut tokens = match itemparser(tokens) {
        ParseResult(t, ParseStatus::Good(item0)) => {
            result.push(item0);
            t
        }
        ParseResult(_, ParseStatus::NoGo(_)) => {
            return succeed(tokens, result);  // simply return no items
        }
        ParseResult(t, ParseStatus::Fail(e)) => return ParseResult(t, ParseStatus::Fail(e)),
    };
    // first item is in the vec, go into loop
    loop {
        // try to match separator
        let t = match sepparser(tokens) {
            ParseResult(t, ParseStatus::Good(_)) => t, // carry on
            ParseResult(_, ParseStatus::NoGo(_)) => {
                // we're done
                return succeed(tokens, result);
            }
            ParseResult(t, ParseStatus::Fail(e)) => return ParseResult(t, ParseStatus::Fail(e)),
        };
        // got separator, must match item
        tokens = match itemparser(t) {
            ParseResult(t, ParseStatus::Good(it)) => {
                result.push(it);
                t
            }
            ParseResult(t, ParseStatus::NoGo(_)) => {
                if canfinishwithsep {
                    return succeed(tokens, result);
                } else {
                    return parsefail(t, "Expected, um, item");
                }
            }
            ParseResult(t, ParseStatus::Fail(e)) => return ParseResult(t, ParseStatus::Fail(e)),
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
            ParseResult(t, $crate::recdec::ParseStatus::NoGo(_)) => return ParseResult(t, $errval),
            ParseResult(t, $crate::recdec::ParseStatus::Fail(e)) => return ParseResult(t, Fail(e))
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

// macro to return ok result from function if a call succeeds
// useful for parsing alternatives. Also returns Fail if it
// gets an error, since that indicates an unrecoverable problem.
// nogo means go on to the next option. you may need to recover
// moved-in data from the NoGo case, so this is the expression
// value of the macro. If it's (), then no change
#[macro_export]
macro_rules! alt {
    ($expr:expr, $f:expr) => (match $expr {
        ParseResult(t, ParseStatus::Good(val)) =>
            return ParseResult(t, ParseStatus::Good($f(val))),
        ParseResult(t, ParseStatus::Fail(e)) =>
            return ParseResult(t, ParseStatus::Fail(e)),
        ParseResult(_, ParseStatus::NoGo(n)) => {n}
    })
}

// like alt, except the passed function is more like a
// parser, in that it's tokens->...->ParseResult, which is useful for
// left-associative rules
// again, NoGo data is expression value.
#[macro_export]
macro_rules! alt_tail {
    ($parsed:expr, $tailparser:expr) => {
        match $parsed {
            ParseResult(t, ParseStatus::Good(val)) =>
                // in general, what we want here is to return val if the tailparser
                // returned nogo. that's what nogo_is_good does
                return $tailparser(t, val).nogo_is_good(),
            ParseResult(t, ParseStatus::Fail(e)) =>
                return ParseResult(t, ParseStatus::Fail(e)),
            ParseResult(_, ParseStatus::NoGo(n)) =>{n}
        }
    }
}




// returns an option<T> depending on whether the result of the parse
// is Good or NoGo. if Fail, returns error.
#[macro_export]
macro_rules! maybeparse {
    ($parsecall:expr) => (
        match $parsecall {
            ParseResult(t, $crate::recdec::ParseStatus::Good(val)) => (t, Some(val)),
            ParseResult(t, $crate::recdec::ParseStatus::NoGo(_)) => (t, None),
            ParseResult(t, $crate::recdec::ParseStatus::Fail(e)) => return ParseResult(t, Fail(e))
        }
     )
}
