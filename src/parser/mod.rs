use crate::scanner::*;
use std::iter::Peekable;
use std::io;

pub struct Parser { 
    scanner: Peekable<Scanner>,
}

impl Parser {
    #[cfg(test)]
    fn from_text(test: &str) -> Self {
        Self {
            scanner: Scanner::from_text(test).peekable()
        }
    }

    pub fn from_file(file_path: &str) -> io::Result<Self> {
        Ok(Self {
            scanner: Scanner::from_file(file_path)?.peekable()
        })
    }

    pub fn parse(self) {
        for result in self.scanner {
            match result {
                Ok(token) => println!("{:?}", token),
                Err(err) => println!("{:?}", err),
            }
        }
    }
}
