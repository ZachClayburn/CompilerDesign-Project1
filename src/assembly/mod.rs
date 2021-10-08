mod file;
mod symbols;

use self::{file::AsmFile, symbols::SymbolTable};
use crate::parser::{Program, Statement};

type Result<T> = std::result::Result<T, String>;

pub fn generate_asm(program: Program) -> Result<String> {
    let mut asm_file = AsmFile::new();
    asm_file.exports.push("global main".into());
    asm_file.text.push("main:".into());

    let mut symbol_table = SymbolTable::new();
    for statement in program.statements {
        match statement {
            Statement::NumDeclaration(name) => {
                let label = symbol_table.add_number(name)?;
                asm_file.bss.push(format!("{:11} resd 1", label));
            }
        }
    }
    // Set up call to exit
    asm_file.text.push("            mov     rax, 60".into());
    asm_file.text.push("            xor     rdi, rdi".into());
    asm_file.text.push("            syscall".into());
    Ok(format!("{}", asm_file))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Statement;
    use indoc::indoc;

    #[test]
    fn can_process_minimal_program() {
        let program = Program::new("program".to_string());
        let asm = generate_asm(program).unwrap();
        let expected = indoc! {"
            global main
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "};
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_program_with_uninitialized_num() {
        let mut program = Program::new("program".to_string());
        program.add_statement(Statement::NumDeclaration("num1".to_string()));
        let asm = generate_asm(program).unwrap();
        let expected = indoc! {"
            global main
                        section .bss
            _n_0_num1   resd 1
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "};
        assert_eq!(asm, expected);
    }
}
