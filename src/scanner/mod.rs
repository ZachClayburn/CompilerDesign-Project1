use std::char;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fs, io};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
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

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Complex tokens
    Number {
        content: i64,
        start: Location,
        stop: Location,
    },
    Variable {
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

impl Iterator for Scanner {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut c = self.raw_text.next()?;
        self.location.next_col();
        while c.is_whitespace() {
            if c == '\n' {
                self.location.next_line();
            }
            self.location.next_col();
            c = self.raw_text.next()?;
        }
        match c {
            '(' => return Some(Ok(Token::LParen(self.location))),
            ')' => return Some(Ok(Token::RParen(self.location))),
            '[' => return Some(Ok(Token::LBracket(self.location))),
            ']' => return Some(Ok(Token::RBracket(self.location))),
            '{' => return Some(Ok(Token::LBrace(self.location))),
            '}' => return Some(Ok(Token::RBrace(self.location))),
            ';' => return Some(Ok(Token::Semicolon(self.location))),
            '+' => return Some(Ok(Token::Plus(self.location))),
            '-' => return Some(Ok(Token::Minus(self.location))),
            '*' => return Some(Ok(Token::Star(self.location))),
            '/' => return Some(Ok(Token::Div(self.location))),
            '^' => return Some(Ok(Token::Pow(self.location))),
            ',' => return Some(Ok(Token::Comma(self.location))),
            '<' if self.raw_text.next_if_eq(&'=').is_some() => {
                let next_token = Some(Ok(Token::LessEqual(self.location)));
                self.location.next_col();
                return next_token;
            }
            '<' => return Some(Ok(Token::Less(self.location))),
            '>' if self.raw_text.next_if_eq(&'=').is_some() => {
                let next_token = Some(Ok(Token::GreaterEqual(self.location)));
                self.location.next_col();
                return next_token;
            }
            '>' => return Some(Ok(Token::Greater(self.location))),
            '=' if self.raw_text.next_if_eq(&'=').is_some() => {
                let next_token = Some(Ok(Token::Equal(self.location)));
                self.location.next_col();
                return next_token;
            }
            '=' => return Some(Ok(Token::Assign(self.location))),
            '!' if self.raw_text.next_if_eq(&'=').is_some() => {
                let next_token = Some(Ok(Token::NotEqual(self.location)));
                self.location.next_col();
                return next_token;
            }
            '.' if self.raw_text.next_if_eq(&'.').is_some() => {
                let next_token = Some(Ok(Token::DoubleDot(self.location)));
                self.location.next_col();
                return next_token;
            }
            '.' => return Some(Ok(Token::Dot(self.location))),
            unexpected => {
                return Some(Err((
                    format!(
                        "Unexpected token {}, ({:#X})",
                        unexpected, unexpected as i32
                    ),
                    self.location,
                )))
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
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
        let has_whitespace = ";   \t;
        ;
        ";
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
            Some(Ok(Token::Semicolon(Location { line: 2, column: 9 })))
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
}
