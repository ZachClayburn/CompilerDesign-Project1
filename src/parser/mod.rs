mod program;
mod statement;
mod expression;

use crate::scanner::Scanner;
use std::iter::Peekable;

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
