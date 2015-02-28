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



fn main() {
    let data = String::from_str("foo( )([ (\n4[) ) urk> <%% @ !6& ptr $").into_bytes();
    for t in lex(&data).iter() {
        println!(
            "{:?} ({}:{}): {:?}",
            t.toktype, t.line, t.column,
            String::from_utf8_lossy(t.bytes)
        )
    }
}
