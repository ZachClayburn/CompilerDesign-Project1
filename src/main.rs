mod assembly;
mod parser;
mod scanner;

use crate::assembly::generate_asm;
use crate::parser::parse;
use crate::scanner::Scanner;
use clap::{App, AppSettings, Arg};

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
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .value_name("FILE")
                .takes_value(true)
                .about("The name of the generated assembly file")
                .default_value("a.asm"),
        )
        .arg(
            Arg::new("error")
                .short('e')
                .long("error")
                .value_name("FILE")
                .takes_value(true)
                .about("The name of the error log")
                .default_value("a.log"),
        )
        .get_matches();
    let infile_name = matches.value_of("input").unwrap();
    println!("Output file name: {}", matches.value_of("output").unwrap());
    println!("Error file name: {}", matches.value_of("error").unwrap());

    let scanner = match Scanner::from_file(infile_name) {
        Ok(scanner) => scanner.peekable(),
        Err(err) => {
            // TODO This should actually print to the error file
            println!("{}", err);
            std::process::exit(1);
        }
    };
    let ast = match parse(scanner) {
        Ok(ast) => ast,
        Err(err) => {
            // TODO This should actually print to the error file
            println!("{}", err);
            std::process::exit(1);
        }
    };
    let asm = match generate_asm(ast) {
        Ok(asm) => asm,
        Err(err) => {
            // TODO This should actually print to the error file
            println!("{}", err);
            std::process::exit(1);
        }
    };
    // TODO this should be going to the asm file
    println!("{}", asm);
}
