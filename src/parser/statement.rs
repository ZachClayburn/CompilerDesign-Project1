use super::{process_bad_token, Expression, Parseable, Result};
use crate::scanner::{Scanner, Token};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Statement {
    NumDeclaration(String),
    Assignment(String, Expression),
    // NumAssigningDeclaration,
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
                Ok(Statement::Assignment(content, Expression::parse(scanner)?))
            }
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
    match scanner.next() {
        Some(Ok(Token::Semicolon(_))) => (),
        unexpected => return process_bad_token(unexpected, "Semicolon"),
    };
    Ok(Statement::NumDeclaration(name))
}

#[cfg(test)]
mod test {

    use super::super::Atom;
    use super::*;

    #[test]
    fn can_parse_num_declaration() {
        let mut scan = Scanner::from_text("num abcd;").peekable();
        let assignment = Statement::parse(&mut scan).unwrap();
        assert!(matches!(assignment, Statement::NumDeclaration(name) if name == "abcd"));
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
    }
}
