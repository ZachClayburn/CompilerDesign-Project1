use super::{process_bad_token, Parseable, Result};
use crate::scanner::{Scanner, Token};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Operator(Box<Expression>, Op, Box<Expression>),
    Parenthetical(Box<Expression>),
    Value(Atom),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Power,
    Times,
    Divide,
    Add,
    Subtract,
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    NumberLiteral(i32),
    Variable(String),
    StringLiteral(String),
}

impl Parseable for Expression {
    fn parse(scanner: &mut std::iter::Peekable<crate::scanner::Scanner>) -> super::Result<Self>
    where
        Self: Sized,
    {
        let exp = match scanner.next() {
            Some(Ok(Token::Number {
                content,
                start,
                stop: _,
            })) => {
                let num = match content.parse() {
                    Ok(num) => num,
                    Err(err) => return Err(format!("[{}:{}] {}", start.line, start.column, err)),
                };
                Expression::Value(Atom::NumberLiteral(num))
            }
            Some(Ok(Token::StringLiteral {
                content,
                start: _,
                stop: _,
            })) => Expression::Value(Atom::StringLiteral(content)),
            unexpected => return process_bad_token(unexpected, "number"),
        };
        Ok(exp)
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn can_parse_number_literal() {
        let mut scan = Scanner::from_text("123").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Value(Atom::NumberLiteral(123));
        assert_eq!(output, expected);
    }

    #[test]
    fn produces_nice_error_for_too_large_ints() {
        let mut scan = Scanner::from_text("4294967296").peekable();
        let output = Expression::parse(&mut scan);
        println!("{}", output.as_ref().err().unwrap());
        assert!(
            matches!(output, Err(err) if err == "[1:1] number too large to fit in target type")
        );
    }

    #[test]
    fn can_parse_string_literal() {
        let mut scan = Scanner::from_text(r#""123""#).peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Value(Atom::StringLiteral("123".to_string()));
        assert_eq!(output, expected);
    }
}
