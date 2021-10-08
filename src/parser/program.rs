use super::{process_bad_token, Parseable, Statement};
use crate::scanner::Token;

#[derive(Debug, PartialEq)]
pub struct Program {
    name: String,
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(name: String) -> Self {
        Self {
            name,
            statements: Vec::new(),
        }
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

impl Parseable for Program {
    fn parse(scanner: &mut std::iter::Peekable<crate::scanner::Scanner>) -> super::Result<Self>
    where
        Self: Sized,
    {
        match scanner.next() {
            Some(Ok(Token::Program(_))) => (),
            unexpected => return process_bad_token(unexpected, "program"),
        };
        let name = match scanner.next() {
            Some(Ok(Token::Identifier {
                content,
                start: _,
                stop: _,
            })) => content,
            unexpected => return process_bad_token(unexpected, "identifier"),
        };
        let mut prog = Program::new(name);
        match scanner.next() {
            Some(Ok(Token::Semicolon(_))) => (),
            unexpected => return process_bad_token(unexpected, "semicolon"),
        };
        match scanner.next() {
            Some(Ok(Token::Begin(_))) => (),
            unexpected => return process_bad_token(unexpected, "begin"),
        }
        loop {
            if let Some(Ok(Token::End(_))) = scanner.peek() {
                break;
            }
            prog.add_statement(Statement::parse(scanner)?);
        }
        match scanner.next() {
            Some(Ok(Token::End(_))) => (),
            unexpected => return process_bad_token(unexpected, "end"),
        }
        match scanner.next() {
            Some(Ok(Token::Dot(_))) => (),
            unexpected => return process_bad_token(unexpected, "dot"),
        }
        Ok(prog)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn can_parse_minimal_program() {
        let mut scan = Scanner::from_text("program test_program;begin end.").peekable();
        let tree = Program::parse(&mut scan).unwrap();
        let expected = Program::new("test_program".to_string());
        assert_eq!(tree, expected);
    }

    #[test]
    fn fails_to_parse_empty_file() {
        let mut scan = Scanner::from_text("").peekable();
        let result = Program::parse(&mut scan);
        assert!(matches!(result, Err(msg) if msg == "Unexpected end of file!"))
    }

    #[test]
    fn fails_to_parse_unnamed_program() {
        let mut scan = Scanner::from_text("program;begin end.").peekable();
        let result = Program::parse(&mut scan);
        assert!(matches!(result, Err(msg) if msg == "[1:8] Expected identifier, got Semicolon"))
    }

    #[test]
    fn fails_to_parse_program_without_semicolon_after_name() {
        let mut scan = Scanner::from_text("program test_program begin end.").peekable();
        let result = Program::parse(&mut scan);
        assert!(matches!(result, Err(msg) if msg == "[1:22] Expected semicolon, got Begin"));
    }

    #[test]
    fn fails_to_parse_program_without_begin() {
        let mut scan = Scanner::from_text("program test_program;").peekable();
        let result = Program::parse(&mut scan);
        assert!(matches!(result, Err(msg) if msg == "Unexpected end of file!"))
    }

    #[test]
    fn fails_to_parse_program_without_end() {
        let mut scan = Scanner::from_text("program test_program; begin").peekable();
        let result = Program::parse(&mut scan);
        assert!(matches!(result, Err(msg) if msg == "Unexpected end of file!"))
    }

    #[test]
    fn can_parse_program_with_simple_num_declaration() {
        let mut scan = Scanner::from_text("program test_program;begin num a;end.").peekable();
        let tree = Program::parse(&mut scan).unwrap();
        let mut expected = Program::new("test_program".to_string());
        expected.add_statement(Statement::NumDeclaration("a".to_string()));
        assert_eq!(tree, expected);
    }
}
