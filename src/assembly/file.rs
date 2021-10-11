use std::fmt::Display;

pub struct AsmFile {
    pub exports: Vec<String>,
    pub imports: Vec<String>,
    pub rodata: Vec<String>,
    /// Initialized data
    pub data: Vec<String>,
    /// Uninitialized data
    pub bss: Vec<String>,
    pub text: Vec<String>,
}

impl AsmFile {
    pub fn new() -> Self {
        Self {
            exports: Vec::new(),
            imports: Vec::new(),
            rodata: vec!["section .rodata".into()],
            data: vec!["section .data".into()],
            bss: vec!["section .bss".into()],
            text: vec!["section .text".into()],
        }
    }
}

impl Display for AsmFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exports = self.exports.join("\n") + "\n";
        let imports = if self.imports.is_empty() {
            "".to_string()
        } else {
            self.imports.join("\n") + "\n"
        };
        let rodata = self.rodata.join("\n") + "\n";
        let data = if self.data.len() > 1 {
            self.data.join("\n") + "\n"
        } else {
            "".to_string()
        };
        let bss = if self.bss.len() > 1 {
            self.bss.join("\n") + "\n"
        } else {
            "".to_string()
        };
        let text = self.text.join("\n") + "\n";
        write!(f, "{}{}{}{}{}{}", exports, imports, rodata, data, bss, text)
    }
}
