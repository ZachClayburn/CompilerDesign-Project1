use std::char;
use std::iter::Peekable;
use std::vec::IntoIter;
use std::{fs, io};

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Complex tokens
    Number(i64),
    Variable(String),
    StringLiteral(String),
    // keywords
    Program,
    Begin,
    End,
    Switch,
    Case,
    Default,
    Write,
    Read,
    For,
    To,
    Step,
    Do,
    If,
    Then,
    Else,
    Array,
    Procedure,
    Num,
    String,
    Return,
    // Symbols
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semicolon,
    Assign,
    Plus,
    Minus,
    Star,
    Div,
    Pow,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    Dot,
    DoubleDot,
    Comma,
}

pub struct Scanner {
    raw_text: Peekable<IntoIter<char>>,
}

impl Scanner {
    pub fn from_text(text: &str) -> Self {
        Self {
            raw_text: text.chars().collect::<Vec<_>>().into_iter().peekable(),
        }
    }

    pub fn from_file(file_path: &str) -> io::Result<Self> {
        Ok(Self::from_text(&fs::read_to_string(file_path)?))
    }
}

pub type Result<T> = std::result::Result<T, String>;

impl Iterator for Scanner {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut c = self.raw_text.next()?;
        while c.is_whitespace() {
            c = self.raw_text.next()?;
        }
        match c {
            '(' => return Some(Ok(Token::LParen)),
            ')' => return Some(Ok(Token::RParen)),
            '[' => return Some(Ok(Token::LBracket)),
            ']' => return Some(Ok(Token::RBracket)),
            '{' => return Some(Ok(Token::LBrace)),
            '}' => return Some(Ok(Token::RBrace)),
            ';' => return Some(Ok(Token::Semicolon)),
            '+' => return Some(Ok(Token::Plus)),
            '-' => return Some(Ok(Token::Minus)),
            '*' => return Some(Ok(Token::Star)),
            '^' => return Some(Ok(Token::Pow)),
            ',' => return Some(Ok(Token::Comma)),
            '<' => {
                let next = self.raw_text.next_if_eq(&'=');
                if next.is_some() {
                    return Some(Ok(Token::LessEqual));
                }
                return Some(Ok(Token::Less));
            }
            '>' => {
                let next = self.raw_text.next_if_eq(&'=');
                if next.is_some() {
                    return Some(Ok(Token::GreaterEqual));
                }
                return Some(Ok(Token::Greater));
            }
            '=' => {
                let next = self.raw_text.next_if_eq(&'=');
                if next.is_some() {
                    return Some(Ok(Token::Equal));
                }
                return Some(Ok(Token::Assign));
            }
            '!' => {
                let next = self.raw_text.next_if_eq(&'=');
                if next.is_none() {
                    return Some(Err(format!("Unexpected token {}, ({:#X})", '!', b'!')));
                }
                return Some(Ok(Token::NotEqual))
            }
            '.' => {
                let next = self.raw_text.next_if_eq(&'.');
                if next.is_some() {
                    return Some(Ok(Token::DoubleDot));
                }
                return Some(Ok(Token::Dot));
            }
            unexpected => {
                return Some(Err(format!(
                    "Unexpected token {}, ({:#X})",
                    unexpected, unexpected as i32
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
            Some(Err(report)) => assert!(false, "Failed with error {}", report),
            None => assert!(false, "Ran out of tokens early!"),
        }
    }

    #[test]
    fn can_lex_single_symbol_tokens() {
        let mut scan = Scanner::from_text("()[]{};+-*^,");
        single_token_check(scan.next(), Token::LParen);
        single_token_check(scan.next(), Token::RParen);
        single_token_check(scan.next(), Token::LBracket);
        single_token_check(scan.next(), Token::RBracket);
        single_token_check(scan.next(), Token::LBrace);
        single_token_check(scan.next(), Token::RBrace);
        single_token_check(scan.next(), Token::Semicolon);
        single_token_check(scan.next(), Token::Plus);
        single_token_check(scan.next(), Token::Minus);
        single_token_check(scan.next(), Token::Star);
        single_token_check(scan.next(), Token::Pow);
        single_token_check(scan.next(), Token::Comma);
        assert_eq!(scan.next(), None, "There are still left over tokens");
    }

    #[test]
    fn can_skip_whitespace() {
        let has_whitespace = ";   \t;
        ;
        ";
        let mut scan = Scanner::from_text(has_whitespace);
        assert_eq!(scan.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(scan.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(scan.next(), Some(Ok(Token::Semicolon)));
        assert_eq!(scan.next(), None);
    }

    #[test]
    fn can_lex_one_or_two_character_tokens() {
        let mut scan = Scanner::from_text("< <= > >= = == != . ..");
        assert_eq!(scan.next(), Some(Ok(Token::Less)));
        assert_eq!(scan.next(), Some(Ok(Token::LessEqual)));
        assert_eq!(scan.next(), Some(Ok(Token::Greater)));
        assert_eq!(scan.next(), Some(Ok(Token::GreaterEqual)));
        assert_eq!(scan.next(), Some(Ok(Token::Assign)));
        assert_eq!(scan.next(), Some(Ok(Token::Equal)));
        assert_eq!(scan.next(), Some(Ok(Token::NotEqual)));
        assert_eq!(scan.next(), Some(Ok(Token::Dot)));
        assert_eq!(scan.next(), Some(Ok(Token::DoubleDot)));
        assert_eq!(scan.next(), None, "There are still left over tokens");
    }
}
