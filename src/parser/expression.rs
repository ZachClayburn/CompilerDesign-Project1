use super::{process_bad_token, Parseable};
use crate::scanner::Token;

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
            Some(Ok(Token::Identifier {
                content,
                start: _,
                stop: _,
            })) => Expression::Value(Atom::Variable(content)),

            unexpected => return process_bad_token(unexpected, "number, variable, or string literal"),
        };
        match scanner.peek() {
            Some(Ok(Token::Pow(_))) => {
                scanner.next();
                Ok(Expression::Operator(
                    Box::new(exp),
                    Op::Power,
                    Box::new(Expression::parse(scanner)?),
                ))
            }
            Some(Ok(Token::Star(_))) => {
                scanner.next();
                Ok(Expression::Operator(
                    Box::new(exp),
                    Op::Times,
                    Box::new(Expression::parse(scanner)?),
                ))
            }
            Some(Ok(Token::Div(_))) => {
                scanner.next();
                Ok(Expression::Operator(
                    Box::new(exp),
                    Op::Divide,
                    Box::new(Expression::parse(scanner)?),
                ))
            }
            Some(Ok(Token::Plus(_))) => {
                scanner.next();
                Ok(Expression::Operator(
                    Box::new(exp),
                    Op::Add,
                    Box::new(Expression::parse(scanner)?),
                ))
            }
            Some(Ok(Token::Minus(_))) => {
                scanner.next();
                Ok(Expression::Operator(
                    Box::new(exp),
                    Op::Subtract,
                    Box::new(Expression::parse(scanner)?),
                ))
            }
            _ => Ok(exp),
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn can_parse_number_literal() {
        let mut scan = Scanner::from_text("123").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Value(Atom::NumberLiteral(123));
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
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
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_variables() {
        let mut scan = Scanner::from_text("num1").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Value(Atom::Variable("num1".to_string()));
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }
    #[test]
    fn can_parse_litteral_addition() {
        let mut scan = Scanner::from_text("1 + 2").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Operator(
            Box::new(Expression::Value(Atom::NumberLiteral(1))),
            Op::Add,
            Box::new(Expression::Value(Atom::NumberLiteral(2))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_litteral_subtraction() {
        let mut scan = Scanner::from_text("1 - 2").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Operator(
            Box::new(Expression::Value(Atom::NumberLiteral(1))),
            Op::Subtract,
            Box::new(Expression::Value(Atom::NumberLiteral(2))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_litteral_multiplication() {
        let mut scan = Scanner::from_text("1 * 2").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Operator(
            Box::new(Expression::Value(Atom::NumberLiteral(1))),
            Op::Times,
            Box::new(Expression::Value(Atom::NumberLiteral(2))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_litteral_division() {
        let mut scan = Scanner::from_text("1 / 2").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Operator(
            Box::new(Expression::Value(Atom::NumberLiteral(1))),
            Op::Divide,
            Box::new(Expression::Value(Atom::NumberLiteral(2))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_litteral_power() {
        let mut scan = Scanner::from_text("1 ^ 2").peekable();
        let output = Expression::parse(&mut scan).unwrap();
        let expected = Expression::Operator(
            Box::new(Expression::Value(Atom::NumberLiteral(1))),
            Op::Power,
            Box::new(Expression::Value(Atom::NumberLiteral(2))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }
}
