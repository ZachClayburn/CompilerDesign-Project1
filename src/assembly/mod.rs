mod file;
mod symbols;

use self::{file::AsmFile, symbols::SymbolTable};
use crate::parser::{Atom, Expression, Op, Program, Statement};
use Atom::*;
use Expression::*;
use Op::*;
use Statement::*;

type Result<T> = std::result::Result<T, String>;

pub fn generate_asm(program: Program) -> Result<String> {
    let mut asm_file = AsmFile::new();
    asm_file.exports.push("global main".into());
    asm_file.imports.push("extern printf".into());

    // Add printInt block
    asm_file.text.push("printInt:".into());
    asm_file.text.push("push rsp".into());
    asm_file.text.push("push rbp".into());
    asm_file.text.push("push rax".into());
    asm_file.text.push("push rcx".into());
    asm_file.text.push("mov rdi, numberPrinter".into());
    asm_file.text.push("mov rsi, rax".into());
    asm_file.text.push("xor rax, rax".into());
    asm_file.text.push("call [rel printf wrt ..got]".into());
    asm_file.text.push("pop rcx".into());
    asm_file.text.push("pop rax".into());
    asm_file.text.push("pop rbp".into());
    asm_file.text.push("pop rsp".into());
    asm_file.text.push("ret".into());

    asm_file
        .rodata
        .push(r#"numberPrinter db "%d",0x0d,0x0a,0"#.into());

    asm_file.text.push("main:".into());
    let mut symbol_table = SymbolTable::new();
    for statement in program.statements {
        match statement {
            NumDeclaration(name, Some(Value(NumberLiteral(num_value)))) => {
                let label = symbol_table.add_number(name)?;
                asm_file.data.push(format!("{} dq {}", label, num_value));
            }
            NumDeclaration(name, init) => {
                let label = symbol_table.add_number(name)?;
                asm_file.bss.push(format!("{} resq 1", label));
                if let Some(exp) = init {
                    asm_file
                        .text
                        .append(&mut generate_expression_assignment_assembly(
                            label,
                            &mut symbol_table,
                            exp,
                        )?);
                }
            }
            Assignment(name, exp) => {
                let label = symbol_table.get_number_label(&name)?;
                asm_file
                    .text
                    .append(&mut generate_expression_assignment_assembly(
                        label,
                        &mut symbol_table,
                        exp,
                    )?);
            }
            Write(exp) => match exp {
                Value(Variable(var_name)) => {
                    let label = symbol_table.get_number_label(&var_name)?;
                    asm_file.text.push(format!("mov rax, [qword {}]", label));
                    asm_file.text.push("call printInt".into());
                }
                unexpected => todo!("{:?}", unexpected),
            },
        }
    }
    // Set up call to exit
    asm_file.text.push("mov rax, 60".into());
    asm_file.text.push("xor rdi, rdi".into());
    asm_file.text.push("syscall".into());
    Ok(format!("{}", asm_file))
}

fn generate_expression_assignment_assembly(
    dest_label: String,
    symbol_table: &mut SymbolTable,
    expression: Expression,
) -> Result<Vec<String>> {
    match expression {
        Value(NumberLiteral(value)) => Ok(vec![format!("mov [qword {}], {}", dest_label, value)]),
        Operator(l_exp, Power, r_exp) => {
            let loop_label = symbol_table.get_new_jump_label();
            let break_label = symbol_table.get_new_jump_label();
            let operands = [*l_exp, *r_exp];
            let [base, exponent] = operands.map(|exp| -> Result<String> {
                match exp {
                    Value(NumberLiteral(value)) => Ok(format!("{}", value)),
                    Value(Variable(name)) => {
                        let var_label = symbol_table.get_number_label(&name)?;
                        Ok(format!("[qword {}]", var_label))
                    }
                    unexpected => todo!("{:?}", unexpected),
                }
            });
            Ok(vec![
                format!("mov rbx, {}", base?),
                "mov rax, rbx".into(),
                format!("mov rdx, {}", exponent?),
                "mov rdi, 1".into(),
                format!("{}:", loop_label),
                "inc rdi".into(),
                "cmp rdx, rdi".into(),
                "jl .L2".into(),
                "imul rax, rbx".into(),
                "jmp .L1".into(),
                format!("{}:", break_label),
                format!("mov [qword {}], rax", dest_label),
            ])
        }
        Operator(l_exp, op, r_exp) => {
            let op_instruction = match op {
                Power => todo!(),
                Times => "imul",
                Divide => todo!(),
                Add => "add",
                Subtract => "sub",
            };
            match (*l_exp, *r_exp) {
                (Value(NumberLiteral(lhs)), Value(NumberLiteral(rhs))) => Ok(vec![
                    format!("mov eax, {}", lhs),
                    format!("{} eax, {}", op_instruction, rhs),
                    format!("mov [qword {}], eax", dest_label),
                ]),
                (Value(NumberLiteral(num)), Value(Variable(var)))
                | (Value(Variable(var)), Value(NumberLiteral(num))) => {
                    let var_label = symbol_table.get_number_label(&var)?;
                    Ok(vec![
                        format!("mov eax, [qword {}]", var_label),
                        format!("{} eax, {}", op_instruction, num),
                        format!("mov [qword {}], eax", dest_label),
                    ])
                }
                unexpected => todo!("{:?}", unexpected),
            }
        }
        unexpected => todo!("{:?}", unexpected),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn flatten_lines(input: &str) -> Vec<String> {
        input
            .lines()
            .map(|line| line.split_whitespace().collect::<Vec<&str>>().join(" "))
            .collect()
    }

    #[test]
    fn flatten_lines_works() {
        let output = flatten_lines(indoc! {"
            global main
                        section .bss
            _n_0_num1   resq 1
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall
            "});
        let expected = vec![
            "global main".to_string(),
            "section .bss".into(),
            "_n_0_num1 resq 1".into(),
            "section .text".into(),
            "main:".into(),
            "mov rax, 60".into(),
            "xor rdi, rdi".into(),
            "syscall".into(),
        ];
        assert_eq!(output, expected);
    }

    #[test]
    fn can_process_minimal_program() {
        let program = Program::new("program".to_string());
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_program_with_uninitialized_num() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration("num1".to_string(), None));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .bss
            _n_0_num1       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_program_with_literal_assignment() {
        let mut program = Program::new("".to_string());
        program.add_statement(NumDeclaration("num1".to_string(), None));
        program.add_statement(Assignment("num1".to_string(), Value(NumberLiteral(10))));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .bss
            _n_0_num1       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     [qword _n_0_num1], 10
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_num_declaration_with_literal_assignment() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_num_expression_assignment() {
        let mut program = Program::new("".to_string());
        program.add_statement(NumDeclaration("num1".to_string(), None));
        program.add_statement(Assignment(
            "num1".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(10))),
                Add,
                Box::new(Value(NumberLiteral(10))),
            ),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .bss
            _n_0_num1       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     eax, 10
                            add     eax, 10
                            mov     [qword _n_0_num1], eax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_num_assignment_with_literal_and_variable() {
        let mut program = Program::new("".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        program.add_statement(NumDeclaration("num2".to_string(), None));
        program.add_statement(Assignment(
            "num2".to_string(),
            Operator(
                Box::new(Value(NumberLiteral(10))),
                Add,
                Box::new(Value(Variable("num1".to_string()))),
            ),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .bss
            _n_1_num2       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     eax, [qword _n_0_num1]
                            add     eax, 10
                            mov     [qword _n_1_num2], eax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_num_declaration_with_expression_assignment() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        program.add_statement(NumDeclaration(
            "num2".to_string(),
            Some(Operator(
                Box::new(Value(Variable("num1".to_string()))),
                Add,
                Box::new(Value(NumberLiteral(10))),
            )),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .bss
            _n_1_num2       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     eax, [qword _n_0_num1]
                            add     eax, 10
                            mov     [qword _n_1_num2], eax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_program_with_write_statement() {
        let mut program = Program::new("".to_string());
        program.add_statement(NumDeclaration("num1".to_string(), None));
        program.add_statement(Assignment("num1".to_string(), Value(NumberLiteral(10))));
        program.add_statement(Write(Value(Variable("num1".to_string()))));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .bss
            _n_0_num1       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     [qword _n_0_num1], 10
                            mov     rax, [qword _n_0_num1]
                            call    printInt
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_multiplication_expression() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        program.add_statement(NumDeclaration(
            "num2".to_string(),
            Some(Operator(
                Box::new(Value(Variable("num1".to_string()))),
                Times,
                Box::new(Value(NumberLiteral(10))),
            )),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .bss
            _n_1_num2       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     eax, [qword _n_0_num1]
                            imul    eax, 10
                            mov     [qword _n_1_num2], eax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_subtracation_expression() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        program.add_statement(NumDeclaration(
            "num2".to_string(),
            Some(Operator(
                Box::new(Value(Variable("num1".to_string()))),
                Subtract,
                Box::new(Value(NumberLiteral(10))),
            )),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .bss
            _n_1_num2       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     eax, [qword _n_0_num1]
                            sub     eax, 10
                            mov     [qword _n_1_num2], eax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }

    #[test]
    fn can_process_power_expressions() {
        let mut program = Program::new("program".to_string());
        program.add_statement(NumDeclaration(
            "num1".to_string(),
            Some(Value(NumberLiteral(3))),
        ));
        program.add_statement(NumDeclaration(
            "num2".to_string(),
            Some(Operator(
                Box::new(Value(Variable("num1".to_string()))),
                Power,
                Box::new(Value(NumberLiteral(10))),
            )),
        ));
        let asm: Vec<String> = generate_asm(program)
            .unwrap()
            .lines()
            .map(|x| x.to_string())
            .collect();
        let expected = flatten_lines(indoc! {r#"
            global main
            extern printf
                            section .rodata
            numberPrinter   db "%d",0x0d,0x0a,0
                            section .data
            _n_0_num1       dq 3
                            section .bss
            _n_1_num2       resq 1
                            section .text
            printInt:
                            push    rsp
                            push    rbp
                            push    rax
                            push    rcx
                            mov     rdi, numberPrinter
                            mov     rsi, rax
                            xor     rax, rax
                            call    [rel printf wrt ..got]
                            pop     rcx
                            pop     rax
                            pop     rbp
                            pop     rsp
                            ret
            main:
                            mov     rbx, [qword _n_0_num1]
                            mov     rax, rbx
                            mov     rdx, 10
                            mov     rdi, 1
            .L1:      
                            inc     rdi
                            cmp     rdx, rdi
                            jl      .L2
                            imul    rax, rbx
                            jmp     .L1
            .L2:
                            mov     [qword _n_1_num2], rax
                            mov     rax, 60
                            xor     rdi, rdi
                            syscall
            "#});
        assert_eq!(asm, expected);
    }
}
