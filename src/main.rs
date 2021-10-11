mod assembly;
mod parser;
mod scanner;

use crate::assembly::generate_asm;
use crate::parser::parse;
use crate::scanner::Scanner;
use clap::{App, AppSettings, Arg};
use std::{env, fs};

fn main() {
    let matches = App::new("compiler")
        .version("1.0")
        .author("Zach Clayburn <zachclayburn@gmail.com>")
        .about("Generate x86 assembly from a given source file")
        .setting(AppSettings::ColoredHelp)
        .arg(
            Arg::new("input")
                .about("The source code to be compiled")
                .required(true),
        )
        // .arg(
        //     Arg::new("output")
        //         .short('o')
        //         .long("output")
        //         .value_name("FILE")
        //         .takes_value(true)
        //         .about("The name of the generated assembly file")
        //         .default_value("a.asm"),
        // )
        // .arg(
        //     Arg::new("error")
        //         .short('e')
        //         .long("error")
        //         .value_name("FILE")
        //         .takes_value(true)
        //         .about("The name of the error log")
        //         .default_value("a.log"),
        // )
        .get_matches();
    let infile_name = matches.value_of("input").unwrap();

    let scanner = match Scanner::from_file(infile_name) {
        Ok(scanner) => scanner.peekable(),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    let ast = match parse(scanner) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };

    let mut out = match env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1)
        }
    };
    out.push(ast.get_name());
    let out_file = out.with_extension("asm");
    let err_file = out.with_extension("err");

    let asm = match generate_asm(ast) {
        Ok(asm) => asm,
        Err(err) => {
            eprintln!("{}", err);
            if let Err(err) = fs::write(err_file, format!("{}", err)) {
                eprintln!("could not write to error file: {}", err)
            }
            std::process::exit(1);
        }
    };
    if let Err(err) = fs::write(out_file, asm) {
        eprintln!("Could not write to output: {}", err);
    }
}
