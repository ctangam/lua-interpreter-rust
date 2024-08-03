use std::collections::HashMap;

use crate::{bytecode::ByteCode, parse::ParseProto, value::Value};


fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[1]);
    0
}

#[derive(Debug)]
pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert(String::from("print"), Value::Function(lib_print));

        ExeState {
            globals,
            stack: Vec::new(),
        }
    }

    pub fn execute(&mut self, proto: &ParseProto) {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, idx) => {
                    let name = &proto.constants[idx as usize];
                    if let Value::String(key) = name {
                        let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                        self.set_stack(dst, v);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::LoadConst(dst, idx) => {
                    let value = proto.constants[idx as usize].clone();
                    self.set_stack(dst, value);
                }
                ByteCode::Call(func, _) => {
                    let func = &self.stack[func as usize];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}");
                    }
                }
            }
        }
    }

    pub fn set_stack(&mut self, dst: u8, value: Value) {
        self.stack.insert(dst as usize, value);
    } 
}
