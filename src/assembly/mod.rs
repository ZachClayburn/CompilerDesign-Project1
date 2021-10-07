use crate::parser::Program;

type Result<T> = std::result::Result<T, String>;

pub fn generate_asm(program: Program) -> Result<String> {
    let mut lines = vec!["global main".to_string()];
    lines.push("            section .text".into());
    lines.push("main:".into());
    lines.push("            mov     rax, 60".into());
    lines.push("            xor     rdi, rdi".into());
    lines.push("            syscall".into());
    Ok(lines.join("\n"))
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    #[test]
    fn can_process_minimal_program() {
        let program = Program::new("program".to_string());
        let asm = generate_asm(program);
        let expected = indoc! {"
            global main
                        section .text
            main:
                        mov     rax, 60
                        xor     rdi, rdi
                        syscall"};
        assert_eq!(asm.unwrap(), expected);
    }
}
