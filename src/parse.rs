use std::fs::File;

use crate::{bytecode::ByteCode, lex::{Lex, Token}, value::Value};


pub struct ParseProto {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>   
}

pub fn load(input: File) -> ParseProto {
    let mut constants = Vec::new();
    let mut byte_codes = Vec::new();
    let mut lex = Lex::new(input);

    loop {
        match lex.do_next() {
            Token::Name(name) => {
                constants.push(Value::String(name));
                byte_codes.push(ByteCode::GetGlobal(0, (constants.len() - 1) as u8));

                if let Token::String(s) = lex.do_next() {
                    constants.push(Value::String(s));
                    byte_codes.push(ByteCode::LoadConst(1, (constants.len() - 1) as u8));
                    byte_codes.push(ByteCode::Call(0, 1));
                } else {
                    panic!("expected string")
                }
            }   
            Token::Eos => break,
            t => panic!("unexpected token: {t:?}"),
        }
    }

    dbg!(&constants);
    dbg!(&byte_codes);
    ParseProto {
        constants,
        byte_codes
    }
}