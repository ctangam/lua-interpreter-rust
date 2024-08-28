use std::env;
use std::fs::File;
use std::io::BufReader;

use parse::ParseProto;

mod value;
mod bytecode;
mod lex;
mod parse;
mod vm;
mod utils;


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} script", args[0]);
        return;
    }

    // let stdin = std::io::stdin();
    // let proto = ParseProto::load(stdin);
    // let input = std::io::Cursor::new("print \"i am from string!\"");  // 字符串+Cursor
    // let proto = ParseProto::load(input);

    let file = File::open(&args[1]).unwrap();
    let input = BufReader::new(file);
    let proto = parse::load(input);

    vm::ExeState::new().execute(&proto, &Vec::new());
}
