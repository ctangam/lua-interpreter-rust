use std::{collections::HashMap, io::Read};

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

    pub fn execute<R: Read>(&mut self, proto: &ParseProto<R>) {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, idx) => {
                    let key: &str = (&proto.constants[idx as usize]).into();
                    let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::SetGlobal(dst, src) => {
                    let key = &proto.constants[dst as usize];
                    let new_value = self.stack[src as usize].clone();
                    self.globals.insert(key.into(), new_value);
                }
                ByteCode::SetGlobalConst(dst, src) => {
                    let key = &proto.constants[dst as usize];
                    let new_value = proto.constants[src as usize].clone();
                    self.globals.insert(key.into(), new_value);
                }
                ByteCode::SetGlobalGlobal(dst, src) => {
                    let key = &proto.constants[dst as usize];
                    let src: &str = (&proto.constants[src as usize]).into();
                    let new_value = self.globals.get(src).unwrap_or(&Value::Nil).clone();
                    self.globals.insert(key.into(), new_value);
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
