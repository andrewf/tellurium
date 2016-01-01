use std::fmt::Debug;
extern crate regex;

#[derive(Debug, Clone)]
pub struct Token<'a, T: Copy> {
    pub toktype: T,
    pub line: usize,
    pub column: usize,
    pub text: &'a str,
}

pub enum LexSpec<'b> {
    Lit(&'b str),
    Re(regex::Regex),
}

// takes an input and a lexspec, and returns length of token match, or 0 if no match
// with re this will be index of end of match, not length of matched text
// we have to have manageable behavior if regex is not anchored to ^
fn spec_match<'a, 'b>(input: &'a str, spec: &'b LexSpec<'b>) -> usize {
    match spec {
        &LexSpec::Lit(s) => {
            let l = s.len();
            if input.len() >= l && s == &input[..l] {
                l
            } else {
                0
            }
        }
        &LexSpec::Re(ref re) => {
            match re.find(input) {
                Some((_, end)) => end,
                None => 0,
            }
        }
    }
}

pub struct TokensIterator<'a, 'b, T>
    where T: Copy,
          T: 'b
{
    input: &'a str,
    typed_specs: &'b [(T, &'b [LexSpec<'b>])],
    line: usize,
    column: usize,
}

impl<'a, 'b, T: Copy> Iterator for TokensIterator<'a, 'b, T> where T: Debug
{
    type Item = Token<'a, T>;

    fn next(&mut self) -> Option<Token<'a, T>> {
        if self.input.len() > 0 {
            for &(ref toktype, ref specs) in self.typed_specs.iter() {
                // if anything matches here, use toktype for token
                for spec in specs.iter() {
                    let l = spec_match(self.input, spec);
                    if l > 0 {
                        // got a match
                        let matched = &self.input[..l];
                        let (oldline, oldcol) = (self.line, self.column);
                        // move input
                        self.input = &self.input[l..];
                        // update line, column statistics
                        let lines = matched.chars().filter(|c| *c == '\n').count();
                        self.line += lines;
                        // cols = matched.len - (index of last \n)
                        // this is still pretty bad wrt unicode
                        if lines > 0 {
                            let (i, _) = matched.char_indices()
                                                .filter(|&(_, c)| c == '\n')
                                                .last()
                                                .unwrap(); // already checked, this iterator
                                                           // should be non-empty
                            self.column = l - i
                        } else {
                            self.column += l
                        }
                        // done
                        // println!("lexed {:?} {:?} {}:{}", *toktype, matched, oldline, oldcol);
                        return Some(Token {
                            toktype: *toktype,
                            line: oldline,
                            column: oldcol,
                            text: matched,
                        });
                    }
                    // else, continue to next pattern
                }
            }
            // tried all patterns, we're toast
            panic!("Unable to lex, line {}, col {}", self.line, self.column);
        } else {
            return None;
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // if there's input left we have at least one
        // more token to go. No clue on the upper bound,
        // not without an average token size
        (if self.input.len() > 0 {
            1
        } else {
            0
        },
         None)
    }
}

pub fn lex<'a, 'b, T: Copy>(input: &'a str,
                            typed_specs: &'b [(T, &'b [LexSpec<'b>])])
                            -> TokensIterator<'a, 'b, T> {
    TokensIterator {
        input: input,
        typed_specs: typed_specs,
        line: 1,
        column: 1,
    }
}
