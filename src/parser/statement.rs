use super::{process_bad_token, Parseable, Result};
use crate::scanner::{Scanner, Token};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Statement {
    NumDeclaration(String),
    // NumAssignment,
    // NumAssigningDeclaration,
}

impl Parseable for Statement {
    fn parse(scanner: &mut Peekable<Scanner>) -> Result<Self>
    where
        Self: Sized,
    {
        match scanner.next() {
            Some(Ok(Token::Num(_))) => num_declarations(scanner),
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

    use super::*;

    #[test]
    fn can_parse_num_declaration() {
        let mut scan = Scanner::from_text("num abcd;").peekable();
        let assignment = Statement::parse(&mut scan).unwrap();
        assert!(matches!(assignment, Statement::NumDeclaration(name) if name == "abcd"));
    }
}
