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
    label_count: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            number_count: 0,
            string_count: 0,
            label_count: 0,
        }
    }

    /// Adds a number to the symbol table and returns the label used to acces it
    pub fn add_number(&mut self, name: String) -> Result<String> {
        let label = format!("_n_{}_{}", self.number_count, name);
        let symbol = Symbol::Number(SymbolInfo{
            label: label.clone(),
        });
        self.add_variable(&name, symbol)?;
        self.number_count += 1;
        Ok(label)
    }

    /// Get the label assigned to a particular number variable
    pub fn get_number_label(&self, var_name: &String) -> Result<String> {
        match self.symbols.get(var_name) {
            Some(Symbol::Number(SymbolInfo { label })) => Ok(label.to_string()),
            Some(Symbol::String(SymbolInfo { label: _ })) => Err(format!(
                "Variable {} used as a number, is actually a string!",
                var_name
            )),
            None => Err(format!("Unkonwn variable {}", var_name)),
        }
    }

    /// Adds a string to the symbol table and returns the label used to acces it
    pub fn add_string(&mut self, name: String) -> Result<String> {
        let label = format!("_s_{}_{}", self.number_count, name);
        let symbol = Symbol::String(SymbolInfo{
            label: label.clone(),
        });
        self.add_variable(&name, symbol)?;
        self.string_count += 1;
        Ok(label)
    }

    /// Adds an anonymous string to the symbol table and returns the label used to acces it
    pub fn add_anonymous_string(&mut self) -> Result<String> {
        let label = format!("_s_{}_", self.number_count);
        self.string_count += 1;
        Ok(label)
    }

    /// Get the label assigned to a particular number variable
    pub fn get_string_label(&self, var_name: &String) -> Result<String> {
        match self.symbols.get(var_name) {
            Some(Symbol::String(SymbolInfo { label })) => Ok(label.to_string()),
            Some(Symbol::Number(SymbolInfo { label: _ })) => Err(format!(
                "Variable {} used as a string, is actually a number!",
                var_name
            )),
            None => Err(format!("Unkonwn variable {}", var_name)),
        }
    }

    fn add_variable(&mut self,name: &String, variable: Symbol) -> Result<String> {
        // TODO This will be where I need to do uniqueness verification
        self.symbols.insert(
            name.to_string(),
            variable
        );
        Ok("".to_string())
    }

    pub fn get_new_jump_label(&mut self) -> String {
        self.label_count += 1;
        format!(".L{}", self.label_count)
    }
}
