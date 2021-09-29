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
    GreterEqual,
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
        let c = self.raw_text.next()?;
        match c {
            '(' => return Some(Ok(Token::LParen)),
            ')' => return Some(Ok(Token::RParen)),
            '[' => return Some(Ok(Token::LBracket)),
            ']' => return Some(Ok(Token::RBracket)),
            '{' => return Some(Ok(Token::LBrace)),
            '}' => return Some(Ok(Token::RBrace)),
            ';' => return Some(Ok(Token::Semicolon)),
            '=' => return Some(Ok(Token::Assign)),
            '+' => return Some(Ok(Token::Plus)),
            '-' => return Some(Ok(Token::Minus)),
            '*' => return Some(Ok(Token::Star)),
            '^' => return Some(Ok(Token::Pow)),
            '<' => return Some(Ok(Token::Less)),
            '>' => return Some(Ok(Token::Greater)),
            ',' => return Some(Ok(Token::Comma)),
            unexpected => return Some(Err(format!("Unexpected token {}", unexpected))),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn can_create_scanner_from_text() {
        Scanner::from_text("Here is some text!");
    }

    #[test]
    fn creating_a_scanner_from_non_existing_file_panics() {
        let scan = Scanner::from_file("Not_A_Real_File.txt");
        assert!(scan.is_err())
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
        let mut scan = Scanner::from_text("()[]{};=+-*^<>,");
        single_token_check(scan.next(), Token::LParen);
        single_token_check(scan.next(), Token::RParen);
        single_token_check(scan.next(), Token::LBracket);
        single_token_check(scan.next(), Token::RBracket);
        single_token_check(scan.next(), Token::LBrace);
        single_token_check(scan.next(), Token::RBrace);
        single_token_check(scan.next(), Token::Semicolon);
        single_token_check(scan.next(), Token::Assign);
        single_token_check(scan.next(), Token::Plus);
        single_token_check(scan.next(), Token::Minus);
        single_token_check(scan.next(), Token::Star);
        single_token_check(scan.next(), Token::Pow);
        single_token_check(scan.next(), Token::Less);
        single_token_check(scan.next(), Token::Greater);
        single_token_check(scan.next(), Token::Comma);
        assert_eq!(scan.next(), None, "There are still left over tokens");
    }
}
