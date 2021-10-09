use super::{process_bad_token, Expression, Parseable, Result};
use crate::scanner::{Scanner, Token};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Statement {
    NumDeclaration(String, Option<Expression>),
    Assignment(String, Expression),
    Write(Expression),
}

impl Parseable for Statement {
    fn parse(scanner: &mut Peekable<Scanner>) -> Result<Self>
    where
        Self: Sized,
    {
        match scanner.next() {
            Some(Ok(Token::Num(_))) => num_declarations(scanner),
            Some(Ok(Token::Identifier {
                content,
                start: _,
                stop: _,
            })) if scanner
                .next_if(|n| matches!(n, Ok(Token::Assign(_))))
                .is_some() =>
            {
                let assignment = Ok(Statement::Assignment(content, Expression::parse(scanner)?));
                match scanner.next() {
                    Some(Ok(Token::Semicolon(_))) => assignment,
                    unexpected => process_bad_token(unexpected, "semicolon"),
                }
            }
            Some(Ok(Token::Write(_))) => {
                let write_statement = Ok(Statement::Write(Expression::parse(scanner)?));

                match scanner.next() {
                    Some(Ok(Token::Semicolon(_))) => write_statement,
                    unexpected => process_bad_token(unexpected, "semicolon"),
                }
            },
            unexpected => process_bad_token(unexpected, "Num"),
        }
    }
}

fn num_declarations(scanner: &mut Peekable<Scanner>) -> Result<Statement> {
    let name = match scanner.next() {
        Some(Ok(Token::Identifier {
            content,
            start: _,
            stop: _,
        })) => content,
        unexpected => return process_bad_token(unexpected, "Identifier"),
    };
    let exp = if let Some(Ok(Token::Assign(_))) = scanner.peek() {
        scanner.next();
        Some(Expression::parse(scanner)?)
    } else {
        None
    };
    match scanner.next() {
        Some(Ok(Token::Semicolon(_))) => (),
        unexpected => return process_bad_token(unexpected, "Semicolon"),
    };
    Ok(Statement::NumDeclaration(name, exp))
}

#[cfg(test)]
mod test {

    use super::super::Atom;
    use super::*;

    #[test]
    fn can_parse_num_declaration() {
        let mut scan = Scanner::from_text("num abcd;").peekable();
        let assignment = Statement::parse(&mut scan).unwrap();
        assert!(matches!(assignment, Statement::NumDeclaration(name, None) if name == "abcd"));
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_simple_string_assignment() {
        let mut scan = Scanner::from_text(r#"a = "1";"#).peekable();
        let output = Statement::parse(&mut scan).unwrap();
        let expected = Statement::Assignment(
            "a".to_string(),
            Expression::Value(Atom::StringLiteral("1".to_string())),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parsre_num_assignment_and_initialization() {
        let mut scan = Scanner::from_text("num a = 1;").peekable();
        let output = Statement::parse(&mut scan).unwrap();
        let expected = Statement::NumDeclaration(
            "a".to_string(),
            Some(Expression::Value(Atom::NumberLiteral(1))),
        );
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }

    #[test]
    fn can_parse_write_with_string_literal() {
        let mut scan = Scanner::from_text(r#"write "hello.txt";"#).peekable();
        let output = Statement::parse(&mut scan).unwrap();
        let expected = Statement::Write(Expression::Value(Atom::StringLiteral(
            "hello.txt".to_string(),
        )));
        assert_eq!(output, expected);
        assert!(
            matches!(scan.next(), None),
            "Tokens still left in the scanner!"
        );
    }
}
