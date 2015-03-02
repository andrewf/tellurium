
use lexer::Token;

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    line: usize,
    column: usize
}

pub type Cursor<'a> = &'a[Token<'a>];
pub type ParseResult<'a, T> = Result<(Cursor<'a>, T), ParseError>;

pub fn expect<'a, F: Fn(&Token<'a>)->bool, S: ToString >(tokens: Cursor<'a>, msg: S, f: F)
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

pub fn expect_word<'a, S: ToString>(tokens: Cursor<'a>, msg: S, expected_text: &str)
                -> ParseResult<'a, Token<'a>>
{
    // to_string here is a kludge to avoid moving msg so we can use it later
    expect(tokens, msg, |t| {t.text == expected_text})
}

pub fn peek_pred<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: &F) -> bool {
    tokens.len() > 0 && f(&tokens[0])
}

// if the next token matches f, pop it off, otherwise do nothing
//fn ignore<'a, F: Fn(&Token<'a>)->bool>(tokens: Cursor<'a>, f: F) -> Cursor<'a> {
//    if tokens.len() > 0 && f(&tokens[0]) {
//        &tokens[1..]
//    } else {
//        tokens
//    }
//}

// skip stuff in the stream. Can't fail, so doesn't return Result,
// just advances cursor
pub fn ignore_many<'a, F: Fn(&Token<'a>)->bool>(mut tokens: Cursor<'a>, f: F) -> Cursor<'a> {
    while peek_pred(tokens, &f) {
        tokens = &tokens[1..];
    }
    tokens
}

// automate the "let (tokens, real_result) = try!(parsefn(tokens, blah blah))" pattern
#[macro_export]
macro_rules! parse {
    // here, token list is only argument
    // let (tokens, r) = try!(f(tokens))
    ( $r:pat = $f:ident ( $token_ident:ident ) ) => {
        let ($token_ident, $r) = try!( $f($token_ident) )
    };
    // also other args to parse function
    // let (tokens, r) = try!(f(tokens, args...))
    ( $r:pat = $f:ident ( $token_ident:ident, $($a:expr),* ) ) => {
        let ($token_ident, $r) = try!( $f($token_ident, $($a),* ) )
    }
}


