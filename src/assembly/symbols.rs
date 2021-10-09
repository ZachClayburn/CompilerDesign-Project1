use super::Result;
use std::collections::HashMap;

struct SymbolInfo {
    label: String,
}

enum Symbol {
    Number(SymbolInfo),
    String(SymbolInfo),
}

pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    number_count: usize,
    string_count: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            number_count: 0,
            string_count: 0,
        }
    }

    pub fn add_number(&mut self, name: String) -> Result<String> {
        let label = format!("_n_{}_{}", self.number_count, name);
        self.symbols.insert(
            name,
            Symbol::Number(SymbolInfo {
                label: label.clone(),
            }),
        );
        self.number_count += 1;
        Ok(label)
    }

    pub fn get_number_label(&mut self, var_name: &String) -> Result<&String> {
        match self.symbols.get(var_name) {
            Some(Symbol::Number(SymbolInfo { label })) => Ok(label),
            Some(Symbol::String(SymbolInfo { label: _ })) => Err(format!(
                "Variable {} used as a number, is actually a string!",
                var_name
            )),
            None => Err(format!("Unkonwn variable {}", var_name)),
        }
    }
}