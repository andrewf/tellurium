
use std::char::CharExt;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub line: usize,
    pub column: usize,
    pub text: &'a str
}

// fixed_tokens are tried in order. put the longest first
pub fn lex<'a, 'b>(mut input: &'a str, fixed_tokens: &[&'b str])
          -> Result<Vec<Token<'a>>, Token<'a>> {
    let mut line = 1;
    let mut col = 1;
    let mut ret = Vec::new();
    while input.len() > 0 {
        // this var is also used to tell whether a token
        // has been recogized so far
        let mut chars_consumed = 0;
        // check for newline (for linecount purposes)
        let incr_line = input.char_at(0) == '\n';
        // look for specific tokens
        for s in fixed_tokens.iter() {
            let toklen = s.len();
            if input.len() >= toklen && (&input[..toklen] == *s) {
                // we got a hit
                chars_consumed = toklen;
                ret.push(Token{
                            line: line,
                            column: col,
                            text: &input[..chars_consumed]
                });
                break;
            }
        }
        // try to do variable tokens (idents, keywords, numbers, etc
        if chars_consumed == 0 {
            // alnum, basically
            while chars_consumed < input.len() &&
                  input.char_at(chars_consumed).is_alphanumeric()
            {
                chars_consumed += 1
            }
            if chars_consumed > 0 {
                ret.push(Token{ line: line, column: col, text: &input[..chars_consumed]});
            }
        }
        // handle whitespace, only if no "real" tokens were recognized
        // because at least newline is a real token, which we don't
        // want to ignore here
        if chars_consumed == 0 && input.char_at(0).is_whitespace() {
            // todo chars_consumed != column delta for a tab?
            chars_consumed = 1;
        }
        if chars_consumed == 0 {
            println!("unexpected char {}", input.char_at(0));
            return Err(Token{line: line, column: col, text: &input[..1]})
        }
        // actually update line no so the newline character itself
        // is tagged with the original line number
        if incr_line {
            line += 1;
            col = 1
        } else {
            col += chars_consumed
        }
        // advance input
        input = &input[chars_consumed..];
    }
    Ok(ret)
}
