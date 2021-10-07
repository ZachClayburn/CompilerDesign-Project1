use crate::parser::Program;

type Result<T> = std::result::Result<T, String>;

pub fn generate_asm(program: Program) -> Result<String> {
    Ok("".to_string())
}
