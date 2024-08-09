use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use crate::{bytecode::ByteCode, parse::ParseProto, value::{Table, Value}};

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
                ByteCode::LoadNil(dst, n) => {
                    for dst in dst..dst + n {
                        self.set_stack(dst, Value::Nil);
                    }
                }
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
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetTable(table, key, value) => {
                    let key = self.stack[key as usize].clone();
                    let value = self.stack[value as usize].clone();
                    if let Value::Table(table) = &self.stack[table as usize] {
                        table.borrow_mut().map.insert(key, value);
                    } else {
                        panic!("invalid table: {table:?}");
                    }
                }
                ByteCode::SetField(table, key, value) => {
                    let key = proto.constants[key as usize].clone();
                    let value = self.stack[value as usize].clone();
                    if let Value::Table(table) = &self.stack[table as usize] {
                        table.borrow_mut().map.insert(key, value);
                    } else {
                        panic!("invalid table: {table:?}");
                    }
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = table as usize + 1;
                    if let Value::Table(table) = self.stack[table as usize].clone() {
                        let values = self.stack.drain(ivalue..ivalue + n as usize);
                        table.borrow_mut().array.extend(values);
                    } else {
                        panic!("invalid table: {table:?}");
                    }
                }
                ByteCode::SetInt(_, _, _) => todo!(),
                ByteCode::SetTableConst(_, _, _) => todo!(),
                ByteCode::SetFieldConst(_, _, _) => todo!(),
                ByteCode::SetIntConst(_, _, _) => todo!(),
                ByteCode::GetTable(_, _, _) => todo!(),
                ByteCode::GetField(_, _, _) => todo!(),
                ByteCode::GetInt(_, _, _) => todo!(),
            }
        }
    }

    pub fn set_stack(&mut self, dst: u8, value: Value) {
        self.stack.insert(dst as usize, value);
    }
}
