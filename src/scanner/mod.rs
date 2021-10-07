use peeking_take_while::PeekableExt;
use std::char;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fs, io, fmt::Display};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 0 }
    }
}

impl Location {
    pub fn next_col(&mut self) {
        self.column += 1;
    }

    pub fn advance_col(&mut self, count: usize) {
        self.column += count;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    pub fn advance_line(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        self.line += count;
        self.column = 0;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Complex tokens
    Number {
        content: String,
        start: Location,
        stop: Location,
    },
    Identifier {
        content: String,
        start: Location,
        stop: Location,
    },
    StringLiteral {
        content: String,
        start: Location,
        stop: Location,
    },
    // keywords
    Program(Location),
    Begin(Location),
    End(Location),
    Switch(Location),
    Case(Location),
    Default(Location),
    Write(Location),
    Read(Location),
    For(Location),
    To(Location),
    Step(Location),
    Do(Location),
    If(Location),
    Then(Location),
    Else(Location),
    Array(Location),
    Procedure(Location),
    Num(Location),
    String(Location),
    Return(Location),
    // Symbols
    LParen(Location),
    RParen(Location),
    LBracket(Location),
    RBracket(Location),
    LBrace(Location),
    RBrace(Location),
    Semicolon(Location),
    Assign(Location),
    Plus(Location),
    Minus(Location),
    Star(Location),
    Div(Location),
    Pow(Location),
    Less(Location),
    Greater(Location),
    LessEqual(Location),
    GreaterEqual(Location),
    Equal(Location),
    NotEqual(Location),
    Dot(Location),
    DoubleDot(Location),
    Comma(Location),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number {
                content,
                start: _,
                stop: _,
            } => write!(f, "Number: {}", content),
            Token::Identifier {
                content,
                start: _,
                stop: _,
            } => write!(f, "Identifier: {}", content),
            Token::StringLiteral {
                content,
                start: _,
                stop: _,
            } => write!(f, "StringLiteral: {}", content),
            Token::Program(_) => write!(f, "Program"),
            Token::Begin(_) => write!(f, "Begin"),
            Token::End(_) => write!(f, "End"),
            Token::Switch(_) => write!(f, "Switch"),
            Token::Case(_) => write!(f, "Case"),
            Token::Default(_) => write!(f, "Default"),
            Token::Write(_) => write!(f, "Write"),
            Token::Read(_) => write!(f, "Read"),
            Token::For(_) => write!(f, "For"),
            Token::To(_) => write!(f, "To"),
            Token::Step(_) => write!(f, "Step"),
            Token::Do(_) => write!(f, "Do"),
            Token::If(_) => write!(f, "If"),
            Token::Then(_) => write!(f, "Then"),
            Token::Else(_) => write!(f, "Else"),
            Token::Array(_) => write!(f, "Array"),
            Token::Procedure(_) => write!(f, "Procedure"),
            Token::Num(_) => write!(f, "Num"),
            Token::String(_) => write!(f, "String"),
            Token::Return(_) => write!(f, "Return"),
            Token::LParen(_) => write!(f, "LParen"),
            Token::RParen(_) => write!(f, "RParen"),
            Token::LBracket(_) => write!(f, "LBracket"),
            Token::RBracket(_) => write!(f, "RBracket"),
            Token::LBrace(_) => write!(f, "LBrace"),
            Token::RBrace(_) => write!(f, "RBrace"),
            Token::Semicolon(_) => write!(f, "Semicolon"),
            Token::Assign(_) => write!(f, "Assign"),
            Token::Plus(_) => write!(f, "Plus"),
            Token::Minus(_) => write!(f, "Minus"),
            Token::Star(_) => write!(f, "Star"),
            Token::Div(_) => write!(f, "Div"),
            Token::Pow(_) => write!(f, "Pow"),
            Token::Less(_) => write!(f, "Less"),
            Token::Greater(_) => write!(f, "Greater"),
            Token::LessEqual(_) => write!(f, "LessEqual"),
            Token::GreaterEqual(_) => write!(f, "GreaterEqual"),
            Token::Equal(_) => write!(f, "Equal"),
            Token::NotEqual(_) => write!(f, "NotEqual"),
            Token::Dot(_) => write!(f, "Dot"),
            Token::DoubleDot(_) => write!(f, "DoubleDot"),
            Token::Comma(_) => write!(f, "Comma"),
        }
    }
}

impl Token {
    pub fn format_location(&self) -> String {
        match self {
            Token::Number {
                content: _,
                start: loc,
                stop: _,
            }
            | Token::Identifier {
                content: _,
                start: loc,
                stop: _,
            }
            | Token::StringLiteral {
                content: _,
                start: loc,
                stop: _,
            }
            | Token::Program(loc)
            | Token::Begin(loc)
            | Token::End(loc)
            | Token::Switch(loc)
            | Token::Case(loc)
            | Token::Default(loc)
            | Token::Write(loc)
            | Token::Read(loc)
            | Token::For(loc)
            | Token::To(loc)
            | Token::Step(loc)
            | Token::Do(loc)
            | Token::If(loc)
            | Token::Then(loc)
            | Token::Else(loc)
            | Token::Array(loc)
            | Token::Procedure(loc)
            | Token::Num(loc)
            | Token::String(loc)
            | Token::Return(loc)
            | Token::LParen(loc)
            | Token::RParen(loc)
            | Token::LBracket(loc)
            | Token::RBracket(loc)
            | Token::LBrace(loc)
            | Token::RBrace(loc)
            | Token::Semicolon(loc)
            | Token::Assign(loc)
            | Token::Plus(loc)
            | Token::Minus(loc)
            | Token::Star(loc)
            | Token::Div(loc)
            | Token::Pow(loc)
            | Token::Less(loc)
            | Token::Greater(loc)
            | Token::LessEqual(loc)
            | Token::GreaterEqual(loc)
            | Token::Equal(loc)
            | Token::NotEqual(loc)
            | Token::Dot(loc)
            | Token::DoubleDot(loc)
            | Token::Comma(loc) => format!("{}:{}", loc.line, loc.column),
        }
    }
}

/// An iterable struct that produces the tokens of the given file
pub struct Scanner {
    raw_text: Peekable<IntoIter<char>>,
    location: Location,
}

impl Scanner {
    pub fn from_text(text: &str) -> Self {
        Self {
            raw_text: text.chars().collect::<Vec<_>>().into_iter().peekable(),
            location: Location::default(),
        }
    }

    pub fn from_file(file_path: &str) -> io::Result<Self> {
        Ok(Self::from_text(&fs::read_to_string(file_path)?))
    }
}

pub type Result<T> = std::result::Result<T, (String, Location)>;

fn is_newline(c: &char) -> bool {
    c == &'\n'
}

impl Iterator for Scanner {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut c = self.raw_text.next()?;
            self.location.next_col();
            while c.is_whitespace() {
                if is_newline(&c) {
                    self.location.next_line();
                }
                self.location.next_col();
                c = self.raw_text.next()?;
            }
            let token = match c {
                '(' => Ok(Token::LParen(self.location)),
                ')' => Ok(Token::RParen(self.location)),
                '[' => Ok(Token::LBracket(self.location)),
                ']' => Ok(Token::RBracket(self.location)),
                '{' => Ok(Token::LBrace(self.location)),
                '}' => Ok(Token::RBrace(self.location)),
                ';' => Ok(Token::Semicolon(self.location)),
                '+' => Ok(Token::Plus(self.location)),
                '-' if self.raw_text.peek().map_or(true, |x| !x.is_digit(10)) => {
                    Ok(Token::Minus(self.location))
                }
                '*' => Ok(Token::Star(self.location)),
                '/' if self.raw_text.next_if_eq(&'/').is_some() => {
                    while !is_newline(&c) {
                        c = self.raw_text.next()?
                    }
                    self.location.next_line();
                    continue;
                }
                '/' if self.raw_text.next_if_eq(&'*').is_some() => {
                    let start_pos = self.location.clone();
                    self.location.next_col();
                    loop {
                        let next_char = self.raw_text.next();
                        if let None = next_char {
                            return Some(Err((
                                "Unterminated block comment".to_string(),
                                start_pos,
                            )));
                        }
                        self.location.next_col();
                        c = next_char?;
                        if is_newline(&c) {
                            self.location.next_line();
                        }
                        if c == '*' && self.raw_text.next_if_eq(&'/').is_some() {
                            self.location.next_col();
                            break;
                        }
                    }
                    continue;
                }
                '/' => Ok(Token::Div(self.location)),
                '^' => Ok(Token::Pow(self.location)),
                ',' => Ok(Token::Comma(self.location)),
                '<' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::LessEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '<' => (Ok(Token::Less(self.location))),
                '>' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::GreaterEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '>' => (Ok(Token::Greater(self.location))),
                '=' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::Equal(self.location));
                    self.location.next_col();
                    next_token
                }
                '=' => (Ok(Token::Assign(self.location))),
                '!' if self.raw_text.next_if_eq(&'=').is_some() => {
                    let next_token = Ok(Token::NotEqual(self.location));
                    self.location.next_col();
                    next_token
                }
                '.' if self.raw_text.next_if_eq(&'.').is_some() => {
                    let next_token = Ok(Token::DoubleDot(self.location));
                    self.location.next_col();
                    next_token
                }
                '.' => (Ok(Token::Dot(self.location))),
                '"' => {
                    let start = self.location.clone();
                    let content: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x != &'"')
                        .collect();
                    let newlines = content.matches('\n').count();
                    self.location.advance_line(newlines);
                    if let Some((_, tail)) = content.rsplit_once('\n') {
                        self.location.advance_col(tail.len() + 1);
                    } else {
                        self.location.advance_col(content.len() + 1);
                    }
                    if let Some('"') = self.raw_text.next_if_eq(&'"') {
                        Ok(Token::StringLiteral {
                            content,
                            start,
                            stop: self.location,
                        })
                    } else {
                        Err(("Unterminated string!".to_string(), start))
                    }
                }
                number if number.is_digit(10) || number == '-' => {
                    let start = self.location.clone();
                    let first_char = number.to_string();
                    let rest: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x.is_digit(10))
                        .collect();
                    self.location.advance_col(rest.len());
                    let content = first_char + &rest;
                    let next = self.raw_text.peek();
                    if next.map_or(false, |x| x.is_alphabetic()) {
                        Err(("Invalid Number".to_string(), start))
                    } else {
                        Ok(Token::Number {
                            content,
                            start,
                            stop: self.location,
                        })
                    }
                }
                letter if letter.is_ascii_alphabetic() => {
                    let start = self.location.clone();
                    let first_char = letter.to_string();
                    let rest: String = self
                        .raw_text
                        .by_ref()
                        .peeking_take_while(|x| x.is_ascii_alphanumeric() || x == &'_')
                        .collect();
                    self.location.advance_col(rest.len());
                    let content = first_char + &rest;
                    match content.as_str() {
                        "program" => Ok(Token::Program(start)),
                        "begin" => Ok(Token::Begin(start)),
                        "end" => Ok(Token::End(start)),
                        "switch" => Ok(Token::Switch(start)),
                        "case" => Ok(Token::Case(start)),
                        "default" => Ok(Token::Default(start)),
                        "write" => Ok(Token::Write(start)),
                        "read" => Ok(Token::Read(start)),
                        "for" => Ok(Token::For(start)),
                        "to" => Ok(Token::To(start)),
                        "step" => Ok(Token::Step(start)),
                        "do" => Ok(Token::Do(start)),
                        "if" => Ok(Token::If(start)),
                        "then" => Ok(Token::Then(start)),
                        "else" => Ok(Token::Else(start)),
                        "array" => Ok(Token::Array(start)),
                        "procedure" => Ok(Token::Procedure(start)),
                        "num" => Ok(Token::Num(start)),
                        "string" => Ok(Token::String(start)),
                        "return" => Ok(Token::Return(start)),
                        _ => Ok(Token::Identifier {
                            content,
                            start,
                            stop: self.location,
                        }),
                    }
                }
                unexpected => Err((
                    format!(
                        "Unexpected token {}, ({:#X})",
                        unexpected, unexpected as i32
                    ),
                    self.location,
                )),
            };
            return Some(token);
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use indoc::indoc;
    use std::path::PathBuf;

    fn get_test_dir() -> PathBuf {
        [env!("CARGO_MANIFEST_DIR"), "resources", "test"]
            .iter()
            .collect()
    }

    #[test]
    fn can_create_scanner_from_text() {
        Scanner::from_text("Here is some text!");
    }

    #[test]
    fn creating_a_scanner_from_non_existing_file_fails() {
        let mut d = get_test_dir();
        d.push("Not_A_Real_File");
        d.set_extension("txt");
        let scan = Scanner::from_file(&d.into_os_string().into_string().unwrap());
        assert!(scan.is_err())
    }

    #[test]
    fn creating_a_scanner_from_an_existing_file_works() {
        let mut d = get_test_dir();
        d.push("I_AM_A_REAL_FILE");
        d.set_extension("txt");
        let scan = Scanner::from_file(&d.into_os_string().into_string().unwrap());
        assert!(scan.is_ok())
    }

    fn single_token_check(next: Option<Result<Token>>, expected: Token) {
        match next {
            Some(Ok(token)) if (token == expected) => (),
            Some(Ok(token)) => assert!(
                false,
                "Expected {:?}, but found {:?} in stead!",
                expected, token
            ),
            Some(Err((report, location))) => {
                assert!(false, "Failed at {:?} with error {}", location, report)
            }
            None => assert!(false, "Ran out of tokens early!"),
        }
    }

    #[test]
    fn can_lex_single_symbol_tokens() {
        let mut scan = Scanner::from_text("()[]{};+-*/^,");
        single_token_check(scan.next(), Token::LParen(Location { line: 1, column: 1 }));
        single_token_check(scan.next(), Token::RParen(Location { line: 1, column: 2 }));
        single_token_check(
            scan.next(),
            Token::LBracket(Location { line: 1, column: 3 }),
        );
        single_token_check(
            scan.next(),
            Token::RBracket(Location { line: 1, column: 4 }),
        );
        single_token_check(scan.next(), Token::LBrace(Location { line: 1, column: 5 }));
        single_token_check(scan.next(), Token::RBrace(Location { line: 1, column: 6 }));
        single_token_check(
            scan.next(),
            Token::Semicolon(Location { line: 1, column: 7 }),
        );
        single_token_check(scan.next(), Token::Plus(Location { line: 1, column: 8 }));
        single_token_check(scan.next(), Token::Minus(Location { line: 1, column: 9 }));
        single_token_check(
            scan.next(),
            Token::Star(Location {
                line: 1,
                column: 10,
            }),
        );
        single_token_check(
            scan.next(),
            Token::Div(Location {
                line: 1,
                column: 11,
            }),
        );
        single_token_check(
            scan.next(),
            Token::Pow(Location {
                line: 1,
                column: 12,
            }),
        );
        single_token_check(
            scan.next(),
            Token::Comma(Location {
                line: 1,
                column: 13,
            }),
        );
        assert_eq!(scan.next(), None, "There are still left over tokens");
    }

    #[test]
    fn can_skip_whitespace() {
        let has_whitespace = indoc! { ";   \t;
        ;
        " };
        let mut scan = Scanner::from_text(has_whitespace);
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Semicolon(Location { line: 1, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Semicolon(Location { line: 1, column: 6 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Semicolon(Location { line: 2, column: 1 })))
        );
        assert_eq!(scan.next(), None);
    }

    #[test]
    fn can_lex_one_or_two_character_tokens() {
        let mut scan = Scanner::from_text("< <= > >= = == != . ..");
        //                                 1234567890123456789012
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Less(Location { line: 1, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::LessEqual(Location { line: 1, column: 3 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Greater(Location { line: 1, column: 6 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::GreaterEqual(Location { line: 1, column: 8 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Assign(Location {
                line: 1,
                column: 11
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Equal(Location {
                line: 1,
                column: 13
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::NotEqual(Location {
                line: 1,
                column: 16
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Dot(Location {
                line: 1,
                column: 19
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::DoubleDot(Location {
                line: 1,
                column: 21
            })))
        );
        assert_eq!(scan.next(), None, "There are still left over tokens");
    }

    #[test]
    fn can_lex_line_comments() {
        let comment_string = indoc! {"
            //This line is a comment!
            / // That / is not
            //This is as well
        "};
        let mut scan = Scanner::from_text(comment_string);
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Div(Location { line: 2, column: 1 })))
        );
        assert_eq!(scan.next(), None);
    }

    #[test]
    fn can_lex_block_comments() {
        let block_comment_string = indoc! {"
        /* This is a block comment, I should just keep going
         * skiping whatever I see, even if I see / or *
         * in fact the only thing that will stop me is those two 
         * characters back it back likt this */
        /
        /* I should work on a single line */
        = /* I should be able to see tokens before and after me */ =
        /* This is an unterminated block comment, and should produce an error
        "};
        let mut scan = Scanner::from_text(block_comment_string);
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Div(Location { line: 5, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Assign(Location { line: 7, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Assign(Location {
                line: 7,
                column: 60
            })))
        );
        let last_value = scan.next();
        assert!(last_value.is_some());
        assert!(last_value.unwrap().is_err());
    }

    #[test]
    fn can_lex_strings() {
        let string_string = indoc! {r#"
            "This is a string" "The next string will be empty" ""
            "This string spans
            multiple lines"
            "This string is not termintated, and should result in an error
        "#};
        let mut scan = Scanner::from_text(string_string);
        assert_eq!(
            scan.next(),
            Some(Ok(Token::StringLiteral {
                content: "This is a string".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location {
                    line: 1,
                    column: 18
                }
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::StringLiteral {
                content: "The next string will be empty".to_string(),
                start: Location {
                    line: 1,
                    column: 20
                },
                stop: Location {
                    line: 1,
                    column: 50
                }
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::StringLiteral {
                content: "".to_string(),
                start: Location {
                    line: 1,
                    column: 52
                },
                stop: Location {
                    line: 1,
                    column: 53
                }
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::StringLiteral {
                content: "This string spans\nmultiple lines".to_string(),
                start: Location { line: 2, column: 1 },
                stop: Location {
                    line: 3,
                    column: 15
                }
            }))
        );
        let unterminated = scan.next();
        assert!(unterminated.is_some());
        assert!(unterminated.unwrap().is_err());
    }

    #[test]
    fn can_lex_numbers() {
        let mut scan = Scanner::from_text("1 123 -213 12a");
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Number {
                content: "1".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location { line: 1, column: 1 },
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Number {
                content: "123".to_string(),
                start: Location { line: 1, column: 3 },
                stop: Location { line: 1, column: 5 },
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Number {
                content: "-213".to_string(),
                start: Location { line: 1, column: 7 },
                stop: Location {
                    line: 1,
                    column: 10
                },
            }))
        );
        let bad_number = scan.next();
        assert!(bad_number.is_some());
        assert!(bad_number.unwrap().is_err());
    }

    #[test]
    fn can_lex_identifyers() {
        let mut scan = Scanner::from_text("notAKeyword Hasnumbers123 has_underscore");
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Identifier {
                content: "notAKeyword".to_string(),
                start: Location { line: 1, column: 1 },
                stop: Location {
                    line: 1,
                    column: 11,
                }
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Identifier {
                content: "Hasnumbers123".to_string(),
                start: Location {
                    line: 1,
                    column: 13,
                },
                stop: Location {
                    line: 1,
                    column: 25,
                }
            }))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Identifier {
                content: "has_underscore".to_string(),
                start: Location {
                    line: 1,
                    column: 27,
                },
                stop: Location {
                    line: 1,
                    column: 40,
                }
            }))
        );
        assert_eq!(scan.next(), None);
    }

    #[test]
    fn can_lex_keywords() {
        let mut scan = Scanner::from_text(indoc! {"
            program
            begin
            end
            switch
            case
            default
            write
            read
            for
            to
            step
            do
            if
            then
            else
            array
            procedure
            num
            string
            return"
        });
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Program(Location { line: 1, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Begin(Location { line: 2, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::End(Location { line: 3, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Switch(Location { line: 4, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Case(Location { line: 5, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Default(Location { line: 6, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Write(Location { line: 7, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Read(Location { line: 8, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::For(Location { line: 9, column: 1 })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::To(Location {
                line: 10,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Step(Location {
                line: 11,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Do(Location {
                line: 12,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::If(Location {
                line: 13,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Then(Location {
                line: 14,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Else(Location {
                line: 15,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Array(Location {
                line: 16,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Procedure(Location {
                line: 17,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Num(Location {
                line: 18,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::String(Location {
                line: 19,
                column: 1
            })))
        );
        assert_eq!(
            scan.next(),
            Some(Ok(Token::Return(Location {
                line: 20,
                column: 1
            })))
        );
        assert_eq!(scan.next(), None);
    }
}
