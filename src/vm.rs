use std::{cell::RefCell, cmp::Ordering, collections::HashMap, io::Read, rc::Rc};

use crate::{
    bytecode::ByteCode,
    parse::ParseProto,
    value::{Table, Value},
};

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
                ByteCode::LoadConst(dst, idx) => {
                    let value = proto.constants[idx as usize].clone();
                    self.set_stack(dst, value);
                }
                ByteCode::LoadNil(dst, n) => self.fill_stack(dst as usize, n as usize),
                ByteCode::LoadBool(dst, b) => self.set_stack(dst, Value::Boolean(b)),
                ByteCode::LoadInt(dst, i) => self.set_stack(dst, Value::Integer(i as i64)),
                ByteCode::Move(dst, src) => {
                    let value = self.stack[src as usize].clone();
                    self.set_stack(dst, value);
                }

                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(table, key, value) => {
                    let value = self.stack[value as usize].clone();
                    self.set_table_int(table, key as i64, value);
                }
                ByteCode::SetIntConst(table, key, value) => {
                    let value = proto.constants[value as usize].clone();
                    self.set_table_int(table, key as i64, value);
                }
                ByteCode::SetField(table, key, value) => {
                    let key = proto.constants[key as usize].clone();
                    let value = self.stack[value as usize].clone();
                    self.set_table(table, key, value);
                }
                ByteCode::SetFieldConst(table, key, value) => {
					let key = proto.constants[key as usize].clone();
					let value = proto.constants[value as usize].clone();
					self.set_table(table, key, value);
				}

                ByteCode::SetTable(table, key, value) => {
                    let key = self.stack[key as usize].clone();
                    let value = self.stack[value as usize].clone();
					self.set_table(table, key, value);
                }
                ByteCode::SetTableConst(table, key, value) => {
					let key = self.stack[key as usize].clone();
					let value = proto.constants[value as usize].clone();
					self.set_table(table, key, value);
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
				ByteCode::GetInt(dst, table, key) => {
					let value = self.get_table_int(table, key as i64);
					self.set_stack(dst, value);
				}
                ByteCode::GetField(dst, table, key) => {
					let key = &proto.constants[key as usize];
					let value = self.get_table(table, key);
					self.set_stack(dst, value);
				}
                ByteCode::GetTable(dst, table, key) => {
					let key = &self.stack[key as usize];
					let value = self.get_table(table, key);
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
        }
    }

    fn set_stack(&mut self, dst: u8, value: Value) {
        let dst = dst as usize;
        match dst.cmp(&self.stack.len()) {
            Ordering::Equal => self.stack.push(value),
            Ordering::Less => self.stack[dst] = value,
            Ordering::Greater => panic!("stack overflow"),
        }
    }

    fn fill_stack(&mut self, begin: usize, num: usize) {
        let end = begin + num;
        let len = self.stack.len();
        if begin < len {
            self.stack[begin..len].fill(Value::Nil)
        }
        if end > len {
            self.stack.resize(end, Value::Nil)
        }
    }

    fn set_table(&mut self, table: u8, key: Value, value: Value) {
        match &key {
            Value::Integer(i) => self.set_table_int(table, *i, value),
            _ => self.do_set_table(table, key, value),
        }
    }

    fn set_table_int(&mut self, table: u8, key: i64, value: Value) {
        if let Value::Table(table) = &self.stack[table as usize] {
            let mut table = table.borrow_mut();
            if key > 0 && (key < 4 || key < table.array.capacity() as i64 * 2) {
                set_vec(&mut table.array, key as usize - 1, value);
            } else {
                table.map.insert(Value::Integer(key), value);
            }
        } else {
            panic!("invalid table: {table:?}");
        }
    }

    fn do_set_table(&mut self, table: u8, key: Value, value: Value) {
        if let Value::Table(table) = &self.stack[table as usize] {
            table.borrow_mut().map.insert(key, value);
        } else {
            panic!("invalid table: {table:?}");
        }
    }

	fn get_table(&self, table: u8, key: &Value) -> Value {
		match key {
			Value::Integer(i) => self.get_table_int(table, *i),
			_ => self.do_get_table(table, key),
		}
	}

	fn get_table_int(&self, table: u8, key: i64) -> Value {
		if let Value::Table(table) = &self.stack[table as usize] {
			let table = table.borrow();
			table.array.get(key as usize - 1).unwrap_or_else(|| table.map.get(&Value::Integer(key)).unwrap_or(&Value::Nil)).clone()
		} else {
			panic!("invalid table: {table:?}");
		}
	}

	fn do_get_table(&self, table: u8, key: &Value) -> Value {
		if let Value::Table(table) = &self.stack[table as usize] {
			table.borrow().map.get(key).unwrap_or(&Value::Nil).clone()
		} else {
			panic!("invalid table: {table:?}");
		}
	}
}

fn set_vec(vec: &mut Vec<Value>, key: usize, value: Value) {
    match key.cmp(&vec.len()) {
        Ordering::Less => vec[key] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(key, Value::Nil);
            vec.push(value)
        }
    }
}