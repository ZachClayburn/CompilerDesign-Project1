mod file;
mod symbols;

use self::{file::AsmFile, symbols::SymbolTable};
use crate::parser::{Atom, Expression, Program, Statement};
use Atom::*;
use Expression::*;
use Statement::*;

type Result<T> = std::result::Result<T, String>;

pub fn generate_asm(program: Program) -> Result<String> {
    let mut asm_file = AsmFile::new();
    asm_file.exports.push("global main".into());
    asm_file.text.push("main:".into());

    let mut symbol_table = SymbolTable::new();
    for statement in program.statements {
        match statement {
            NumDeclaration(name, Some(Value(NumberLiteral(num_value)))) => {
                let label = symbol_table.add_number(name)?;
                asm_file.data.push(format!("{:11} dd {}", label, num_value));
            }
            NumDeclaration(name, None) => {
                let label = symbol_table.add_number(name)?;
                asm_file.bss.push(format!("{:11} resd 1", label));
            }
            Assignment(name, Value(NumberLiteral(value))) => {
                let label = symbol_table.get_number_label(&name)?;
                asm_file
                    .text
                    .push(format!("            mov     DWORD[{}], {}", label, value));
            }
            unexpected => todo!("{:?}", unexpected),
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
        program.add_statement(NumDeclaration("num1".to_string(), None));
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

    #[test]
    fn can_process_program_with_literal_assignment() {
        let mut program = Program::new("".to_string());
        program.add_statement(NumDeclaration("num1".to_string(), None));
        program.add_statement(Assignment("num1".to_string(), Value(NumberLiteral(10))));
        let asm = generate_asm(program).unwrap();
        let expected = indoc! {"
            global main
                        section .bss
            _n_0_num1   resd 1
                        section .text
            main:
                        mov     DWORD[_n_0_num1], 10
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "};
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_num_declaration_with_liteeral_assignment() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        let asm = generate_asm(program).unwrap();
        let expected = indoc! {"
            global main
                        section .data
            _n_0_num1   dd 3
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "};
        assert_eq!(asm, expected);
    }
}
