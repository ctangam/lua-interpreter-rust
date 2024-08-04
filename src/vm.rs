use std::{collections::HashMap, fmt::DebugList, mem};

use crate::{bytecode::ByteCode, parse::ParseProto, value::Value};

fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[state.func_index + 1]);
    0
}

#[derive(Debug)]
pub struct ExeState {
    pub globals: HashMap<String, Value>,
    pub stack: Vec<Value>,
    pub func_index: usize,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert(String::from("print"), Value::Function(lib_print));

        ExeState {
            globals,
            stack: Vec::new(),
            func_index: 0,
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
                ByteCode::SetGlobal(dst, src) => {
                    let name = proto.constants[dst as usize].clone();
                    if let Value::String(key) = name {
                        let new_value = self.stack[src as usize].clone();
                        self.globals.insert(key, new_value);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::SetGlobalConst(dst, src) => {
                    let name = proto.constants[dst as usize].clone();
                    if let Value::String(key) = name {
                        let new_value = proto.constants[src as usize].clone();
                        self.globals.insert(key, new_value);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::SetGlobalGlobal(dst, src) => {
                    let name = proto.constants[dst as usize].clone();
                    if let Value::String(key) = name {
                        let src = &proto.constants[src as usize];
                        if let Value::String(src) = src {
                            let new_value = self.globals.get(src).unwrap_or(&Value::Nil).clone();
                            self.globals.insert(key, new_value);
                        } else {
                            panic!("invalid global key: {src:?}");
                        }
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::LoadConst(dst, idx) => {
                    let value = proto.constants[idx as usize].clone();
                    self.set_stack(dst, value);
                }
                ByteCode::LoadNil(dst) => self.set_stack(dst, Value::Nil),
                ByteCode::LoadBool(dst, b) => self.set_stack(dst, Value::Boolean(b)),
                ByteCode::LoadInt(dst, i) => self.set_stack(dst, Value::Integer(i as i64)),
                ByteCode::Move(dst, src) => {
                    let value = self.stack[src as usize].clone();
                    self.set_stack(dst, value);
                }
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}");
                    }
                }
            }
            // println!("");
            // println!("{:?}", code);
            // dbg!(&self.stack);
        }
    }

    pub fn set_stack(&mut self, dst: u8, value: Value) {
        self.stack.insert(dst as usize, value);
    }
}
