mod expression;
mod program;
mod statement;

use crate::scanner::Scanner;
use std::iter::Peekable;

pub use expression::{Atom, Expression, Op};
pub use program::Program;
pub use statement::Statement;

pub type Result<T> = std::result::Result<T, String>;

pub fn parse(mut scan: Peekable<Scanner>) -> Result<Program> {
    let tree = Program::parse(&mut scan);
    if let Ok(ref program) = tree {
        println!("Done parsing program {}", program.get_name());
    }
    tree
}

trait Parseable {
    fn parse(scanner: &mut Peekable<Scanner>) -> Result<Self>
    where
        Self: Sized;
}

fn process_bad_token<T>(r: Option<<Scanner as Iterator>::Item>, expected_token: &str) -> Result<T> {
    match r {
        Some(Err((error, location))) => {
            Err(format!("Errror in scanner at {:?}: {}", location, error))
        }
        Some(Ok(token)) => Err(format!(
            "[{}] Expected {}, got {}",
            token.format_location(),
            expected_token,
            token
        )),
        None => Err("Unexpected end of file!".to_string()),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::path::PathBuf;
    use Atom::*;
    use Expression::*;
    use Op::*;
    use Statement::*;

    fn get_test_dir() -> PathBuf {
        [env!("CARGO_MANIFEST_DIR"), "resources", "test"]
            .iter()
            .collect()
    }

    #[test]
    fn can_properly_parse_project_basics_file() {
        let mut path = get_test_dir();
        path.push("basics.txt");
        path.set_extension("txt");
        let scan = Scanner::from_file(&path.into_os_string().into_string().unwrap())
            .unwrap()
            .peekable();
        let ast = parse(scan).unwrap();
        let mut expected = Program::new("basicsExample".to_string());
        expected.add_statement(NumDeclaration("num1".to_string(), None));
        expected.add_statement(NumDeclaration(
            "num2".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        expected.add_statement(NumDeclaration(
            "num3".to_string(),
            Some(Value(Variable("num2".to_string()))),
        ));
        expected.add_statement(Assignment(
            "num3".to_string(),
            Operator(
                Box::new(Value(Variable("num3".to_string()))),
                Add,
                Box::new(Value(NumberLiteral(10))),
            ),
        ));
        expected.add_statement(Assignment(
            "num2".to_string(),
            Operator(
                Box::new(Value(Variable("num3".to_string()))),
                Add,
                Box::new(Value(Variable("num2".to_string()))),
            ),
        ));
        expected.add_statement(Assignment(
            "num1".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(2))),
                Times,
                Box::new(Value(NumberLiteral(5))),
            ),
        ));
        expected.add_statement(Assignment(
            "num1".to_string(),
            Operator(
                Box::new(Value(Variable("num3".to_string()))),
                Times,
                Box::new(Value(Variable("num2".to_string()))),
            ),
        ));
        expected.add_statement(Assignment(
            "num2".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(8))),
                Subtract,
                Box::new(Value(NumberLiteral(5))),
            ),
        ));
        expected.add_statement(Assignment(
            "num2".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(8))),
                Subtract,
                Box::new(Value(NumberLiteral(5))),
            ),
        ));
        expected.add_statement(Assignment(
            "num3".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(8))),
                Power,
                Box::new(Value(NumberLiteral(6))),
            ),
        ));
        expected.add_statement(Write(Value(StringLiteral("Basics.txt:".to_string()))));
        expected.add_statement(Write(Value(Variable("num1".to_string()))));
        expected.add_statement(Write(Value(Variable("num2".to_string()))));
        expected.add_statement(Write(Value(Variable("num3".to_string()))));
        assert_eq!(ast, expected);
    }

    #[test]
    fn can_properly_parse_project_basics_new_file() {
        let mut path = get_test_dir();
        path.push("basics-new.txt");
        path.set_extension("txt");
        let scan = Scanner::from_file(&path.into_os_string().into_string().unwrap())
            .unwrap()
            .peekable();
        let ast = parse(scan).unwrap();
        let mut expected = Program::new("basicsExample_new".to_string());

        expected.add_statement(NumDeclaration(
            "value3".to_string(),
            Some(Value(NumberLiteral(3))),
        ));

        expected.add_statement(NumDeclaration(
            "addresult".to_string(),
            Some(Operator(
                Box::new(Value(NumberLiteral(10))),
                Add,
                Box::new(Value(Variable("value3".to_string()))),
            )),
        ));
        expected.add_statement(Write(Value(Variable("addresult".to_string()))));

        expected.add_statement(NumDeclaration(
            "mulresult".to_string(),
            Some(Operator(
                Box::new(Value(NumberLiteral(10))),
                Times,
                Box::new(Value(Variable("value3".to_string()))),
            )),
        ));
        expected.add_statement(Write(Value(Variable("mulresult".to_string()))));

        expected.add_statement(NumDeclaration(
            "subresult".to_string(),
            Some(Operator(
                Box::new(Value(Variable("value3".to_string()))),
                Subtract,
                Box::new(Value(NumberLiteral(10))),
            )),
        ));
        expected.add_statement(Write(Value(Variable("subresult".to_string()))));

        expected.add_statement(NumDeclaration(
            "expresult".to_string(),
            Some(Operator(
                Box::new(Value(NumberLiteral(10))),
                Power,
                Box::new(Value(Variable("value3".to_string()))),
            )),
        ));
        expected.add_statement(Write(Value(Variable("expresult".to_string()))));

        assert_eq!(ast, expected);
    }
}
