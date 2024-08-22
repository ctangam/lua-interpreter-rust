use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    env::var,
    io::{Read, Write},
    rc::Rc,
};

use crate::{
    bytecode::ByteCode,
    parse::FuncProto,
    utils::ftoi,
    value::{Table, Value},
};

fn lib_print(state: &mut ExeState) -> i32 {
    let narg = state.get_top();
    for i in 0..narg {
        if i != 0 {
            print!("\t")
        }
        print!("{}", state.get::<&Value>(i).to_string());
    }
    println!("");
    0
}

fn lib_type(state: &mut ExeState) -> i32 {
    let ty = state.get::<&Value>(1).ty();
    state.push(ty);
    1
}

#[derive(Debug)]
pub struct ExeState {
    pub globals: HashMap<String, Value>,
    pub stack: Vec<Value>,
    pub base: usize,
}

impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("print".into(), Value::RustFunction(lib_print));
        globals.insert("type".into(), Value::RustFunction(lib_type));

        ExeState {
            globals,
            stack: Vec::new(),
            base: 1,
        }
    }

    pub fn execute(&mut self, proto: &FuncProto) -> usize {
        let vargs = if proto.has_vargs {
            self.stack.drain(self.base + proto.nparam..).collect()
        } else {
            Vec::new()
        };

        let mut pc = 0;
        loop {
            println!("  [{pc}]\t{:?}", proto.byte_codes[pc]);
            match proto.byte_codes[pc] {
                ByteCode::GetGlobal(dst, idx) => {
                    let key: &str = (&proto.constants[idx as usize]).into();
                    let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::SetGlobal(dst, src) => {
                    let key = &proto.constants[dst as usize];
                    let new_value = self.get_stack(src).clone();
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
                    let value = self.get_stack(src).clone();
                    self.set_stack(dst, value);
                }

                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(table, key, value) => {
                    let value = self.get_stack(value).clone();
                    self.set_table_int(table, key as i64, value);
                }
                ByteCode::SetIntConst(table, key, value) => {
                    let value = proto.constants[value as usize].clone();
                    self.set_table_int(table, key as i64, value);
                }
                ByteCode::SetField(table, key, value) => {
                    let key = proto.constants[key as usize].clone();
                    let value = self.get_stack(value).clone();
                    self.set_table(table, key, value);
                }
                ByteCode::SetFieldConst(table, key, value) => {
                    let key = proto.constants[key as usize].clone();
                    let value = proto.constants[value as usize].clone();
                    self.set_table(table, key, value);
                }

                ByteCode::SetTable(table, key, value) => {
                    let key = self.get_stack(key).clone();
                    let value = self.get_stack(value).clone();
                    self.set_table(table, key, value);
                }
                ByteCode::SetTableConst(table, key, value) => {
                    let key = self.get_stack(key).clone();
                    let value = proto.constants[value as usize].clone();
                    self.set_table(table, key, value);
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = self.base + table as usize + 1;
                    if let Value::Table(table) = self.get_stack(table).clone() {
                        let end = if n == 0 {
                            self.stack.len()
                        } else {
                            ivalue + n as usize
                        };
                        let values = self.stack.drain(ivalue..end);
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
                    let key = &self.get_stack(key);
                    let value = self.get_table(table, key);
                    self.set_stack(dst, value);
                }

                ByteCode::Vargs(dst, want) => {
                    self.stack.truncate(self.base + dst as usize);

                    let len = vargs.len();
                    let want = want as usize;
                    if want == 0 {
                        self.stack.extend_from_slice(&vargs);
                    } else if want > len {
                        self.stack.extend_from_slice(&vargs);
                        self.fill_stack(dst as usize + len, want - len);
                    } else {
                        self.stack.extend_from_slice(&vargs[..want]);
                    }
                }
                ByteCode::Call(func, narg_plus, want_nret) => {
                    let nret = self.call_function(func, narg_plus);

                    self.stack
                        .drain(self.base + func as usize..self.stack.len() - nret);

                    let want_nret = want_nret as usize;
                    if nret < want_nret {
                        self.fill_stack(nret, want_nret - nret)
                    }
                }
                ByteCode::CallSet(dst, func, narg) => {
                    let nret = self.call_function(func, narg);

                    if nret == 0 {
                        self.set_stack(dst, Value::Nil)
                    } else {
                        let iret = self.stack.len() - nret;
                        self.stack.swap(self.base + dst as usize, iret);
                    }

                    self.stack.truncate(self.base + func as usize + 1)
                }
                ByteCode::TailCall(func, narg_plus) => {
                    self.stack.drain(self.base - 1..self.base + func as usize);
                    return self.do_call_function(narg_plus);
                }
                ByteCode::Return(iret, nret) => {
                    let iret = self.base + iret as usize;
                    if nret == 0 {
                        return self.stack.len() - iret;
                    } else {
                        self.stack.truncate(iret + nret as usize);
                        return nret as usize;
                    }
                }
                ByteCode::Return0 => return 0,

                ByteCode::Neg(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Integer(i) => Value::Integer(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Not(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Nil => Value::Boolean(true),
                        Value::Boolean(b) => Value::Boolean(!b),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::BitNot(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Integer(i) => Value::Integer(!i),
                        _ => panic!("invalid ~"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Len(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::ShortStr(len, _) => Value::Integer(*len as i64),
                        Value::MidStr(s) => Value::Integer(s.0 as i64),
                        Value::LongStr(s) => Value::Integer(s.len() as i64),
                        Value::Table(t) => Value::Integer(t.borrow().array.len() as i64),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value);
                }

                ByteCode::Add(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &self.get_stack(b),
                        |a, b| a + b,
                        |a, b| a + b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::AddConst(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &proto.constants[b as usize],
                        |a, b| a + b,
                        |a, b| a + b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::AddInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a, b| a + b, |a, b| a + b);
                    self.set_stack(dst, r);
                }
                ByteCode::Sub(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &self.get_stack(b),
                        |a, b| a - b,
                        |a, b| a - b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::SubConst(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &proto.constants[b as usize],
                        |a, b| a - b,
                        |a, b| a - b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::SubInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a, b| a - b, |a, b| a - b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mul(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &self.get_stack(b),
                        |a, b| a * b,
                        |a, b| a * b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::MulConst(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &proto.constants[b as usize],
                        |a, b| a * b,
                        |a, b| a * b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::MulInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a, b| a * b, |a, b| a * b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mod(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &self.get_stack(b),
                        |a, b| a % b,
                        |a, b| a % b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::ModConst(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &proto.constants[b as usize],
                        |a, b| a % b,
                        |a, b| a % b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::ModInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a, b| a % b, |a, b| a % b);
                    self.set_stack(dst, r);
                }
                ByteCode::Idiv(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &self.get_stack(b),
                        |a, b| a / b,
                        |a, b| a / b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::IdivConst(dst, a, b) => {
                    let r = exe_binop(
                        &self.get_stack(a),
                        &proto.constants[b as usize],
                        |a, b| a / b,
                        |a, b| a / b,
                    );
                    self.set_stack(dst, r);
                }
                ByteCode::IdivInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a, b| a / b, |a, b| a / b);
                    self.set_stack(dst, r);
                }
                ByteCode::Div(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &self.get_stack(b), |a, b| a / b);
                    self.set_stack(dst, r);
                }
                ByteCode::DivConst(dst, a, b) => {
                    let r =
                        exe_binop_f(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a / b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::DivInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.get_stack(a), i, |a, b| a / b);
                    self.set_stack(dst, r);
                }
                ByteCode::Pow(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &self.get_stack(b), |a, b| a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::PowConst(dst, a, b) => {
                    let r =
                        exe_binop_f(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a.powf(b)
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::PowInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.get_stack(a), i, |a, b| a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::BitAnd(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a, b| a & b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndConst(dst, a, b) => {
                    let r =
                        exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a & b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a, b| a & b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOr(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a, b| a | b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrConst(dst, a, b) => {
                    let r =
                        exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a | b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a, b| a | b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXor(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a, b| a ^ b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorConst(dst, a, b) => {
                    let r =
                        exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a ^ b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a, b| a ^ b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftL(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a, b| a << b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLConst(dst, a, b) => {
                    let r =
                        exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a << b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a, b| a << b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftR(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a, b| a >> b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRConst(dst, a, b) => {
                    let r =
                        exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a, b| {
                            a >> b
                        });
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a, b| a >> b);
                    self.set_stack(dst, r);
                }

                ByteCode::Concat(dst, a, b) => {
                    let r = exe_concat(&self.get_stack(a), &self.get_stack(b));
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatConst(dst, a, b) => {
                    let r = exe_concat(&self.get_stack(a), &proto.constants[b as usize]);
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatInt(dst, a, i) => {
                    let r = exe_concat(&self.get_stack(a), &Value::Integer(i as i64));
                    self.set_stack(dst, r);
                }
                ByteCode::Test(icond, jmp) => {
                    let cond = &self.get_stack(icond);
                    if matches!(cond, Value::Nil | Value::Boolean(false)) {
                        pc = (pc as isize + jmp as isize) as usize
                    }
                }
                ByteCode::Jump(jmp) => pc = (pc as isize + jmp as isize) as usize,
                ByteCode::ForPrepare(dst, jmp) => {
                    if let (&Value::Integer(mut i), &Value::Integer(step)) =
                        (self.get_stack(dst), self.get_stack(dst + 2))
                    {
                        if step == 0 {
                            panic!("0 step in numerical for")
                        }
                        let limit = match *self.get_stack(dst + 1) {
                            Value::Integer(limit) => limit,
                            Value::Float(limit) => {
                                let limit = for_int_limit(limit, step > 0, &mut i);
                                self.set_stack(dst + 1, Value::Integer(limit));
                                limit
                            }
                            _ => panic!("invalid limit type"),
                        };
                        if !for_check(i, limit, step > 0) {
                            pc += jmp as usize;
                        }
                    } else {
                        let i = self.make_float(dst);
                        let limit = self.make_float(dst + 1);
                        let step = self.make_float(dst + 2);
                        if step == 0.0 {
                            panic!("invalid for numerical exp")
                        }
                        if !for_check(i, limit, step > 0.0) {
                            pc += jmp as usize;
                        }
                    }
                }
                ByteCode::ForLoop(dst, jmp) => match self.get_stack(dst) {
                    Value::Integer(i) => {
                        let limit = self.read_int(dst + 1);
                        let step = self.read_int(dst + 2);
                        let i = i + step;
                        if for_check(i, limit, step > 0) {
                            self.set_stack(dst, Value::Integer(i));
                            pc -= jmp as usize;
                        }
                    }
                    Value::Float(f) => {
                        let limit = self.read_float(dst + 1);
                        let step = self.read_float(dst + 2);
                        let f = f + step;
                        if for_check(f, limit, step > 0.0) {
                            self.set_stack(dst, Value::Float(f));
                            pc -= jmp as usize;
                        }
                    }
                    _ => panic!("xx"),
                },
            }
            pc += 1;
        }
    }

    fn call_function(&mut self, func: u8, narg_plus: u8) -> usize {
        self.base += func as usize + 1;
        let nret = self.do_call_function(narg_plus);
        self.base -= func as usize + 1;
        nret
    }

    fn do_call_function(&mut self, narg_plus: u8) -> usize {
        match self.stack[self.base - 1].clone() {
            Value::RustFunction(f) => {
                if narg_plus != 0 {
                    self.stack.truncate(self.base + narg_plus as usize - 1);
                }
                f(self) as usize
            }
            Value::LuaFunction(f) => {
                let narg = if narg_plus == 0 {
                    self.stack.len() - self.base
                } else {
                    narg_plus as usize - 1
                };

                if narg < f.nparam {
                    self.fill_stack(narg, f.nparam - narg);
                } else if f.has_vargs && narg_plus != 0 {
                    self.stack.truncate(self.base + narg);
                }
                self.execute(&f)
            }
            f => panic!("invalid function: {f:?}"),
        }
    }

    fn make_float(&mut self, dst: u8) -> f64 {
        match *self.get_stack(dst) {
            Value::Float(f) => f,
            Value::Integer(i) => {
                let f = i as f64;
                self.set_stack(dst, Value::Float(f));
                f
            }
            ref v => panic!("not number {v:?}"),
        }
    }

    fn read_int(&self, dst: u8) -> i64 {
        if let Value::Integer(i) = *self.get_stack(dst) {
            i
        } else {
            panic!("invalid integer");
        }
    }
    fn read_float(&self, dst: u8) -> f64 {
        if let Value::Float(f) = *self.get_stack(dst) {
            f
        } else {
            panic!("invalid integer");
        }
    }

    fn get_stack(&self, dst: u8) -> &Value {
        &self.stack[self.base + dst as usize]
    }

    fn set_stack(&mut self, dst: u8, value: Value) {
        set_vec(&mut self.stack, self.base + dst as usize, value)
    }

    fn fill_stack(&mut self, begin: usize, num: usize) {
        let begin = self.base + begin;
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
        if let Value::Table(table) = self.get_stack(table) {
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
        if let Value::Table(table) = self.get_stack(table) {
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
        if let Value::Table(table) = self.get_stack(table) {
            let table = table.borrow();
            table
                .array
                .get(key as usize - 1)
                .unwrap_or_else(|| table.map.get(&Value::Integer(key)).unwrap_or(&Value::Nil))
                .clone()
        } else {
            panic!("invalid table: {table:?}");
        }
    }

    fn do_get_table(&self, table: u8, key: &Value) -> Value {
        if let Value::Table(table) = self.get_stack(table) {
            table.borrow().map.get(key).unwrap_or(&Value::Nil).clone()
        } else {
            panic!("invalid table: {table:?}");
        }
    }
}

impl<'a> ExeState {
    pub fn get_top(&self) -> usize {
        self.stack.len() - self.base
    }

    pub fn get<T>(&'a self, i: usize) -> T
    where
        T: From<&'a Value>,
    {
        (&self.stack[self.base + i - 1]).into()
    }

    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into())
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

fn exe_binop(
    v1: &Value,
    v2: &Value,
    arith_i: fn(i64, i64) -> i64,
    arith_f: fn(f64, f64) -> f64,
) -> Value {
    match (v1, v2) {
        (Value::Integer(i1), Value::Integer(i2)) => Value::Integer(arith_i(*i1, *i2)),
        (Value::Integer(i1), Value::Float(f2)) => Value::Float(arith_f(*i1 as f64, *f2)),
        (Value::Float(f1), Value::Float(f2)) => Value::Float(arith_f(*f1, *f2)),
        (Value::Float(f1), Value::Integer(i2)) => Value::Float(arith_f(*f1, *i2 as f64)),
        (_, _) => todo!("meta"),
    }
}
fn exe_binop_int(
    v1: &Value,
    i2: u8,
    arith_i: fn(i64, i64) -> i64,
    arith_f: fn(f64, f64) -> f64,
) -> Value {
    match v1 {
        Value::Integer(i1) => Value::Integer(arith_i(*i1, i2 as i64)),
        Value::Float(f1) => Value::Float(arith_f(*f1, i2 as f64)),
        _ => todo!("meta"),
    }
}

fn exe_binop_f(v1: &Value, v2: &Value, arith_f: fn(f64, f64) -> f64) -> Value {
    let (f1, f2) = match (v1, v2) {
        (Value::Integer(i1), Value::Integer(i2)) => (*i1 as f64, *i2 as f64),
        (Value::Integer(i1), Value::Float(f2)) => (*i1 as f64, *f2),
        (Value::Float(f1), Value::Float(f2)) => (*f1, *f2),
        (Value::Float(f1), Value::Integer(i2)) => (*f1, *i2 as f64),
        (_, _) => todo!("meta"),
    };
    Value::Float(arith_f(f1, f2))
}
fn exe_binop_int_f(v1: &Value, i2: u8, arith_f: fn(f64, f64) -> f64) -> Value {
    let f1 = match v1 {
        Value::Integer(i1) => *i1 as f64,
        Value::Float(f1) => *f1,
        _ => todo!("meta"),
    };
    Value::Float(arith_f(f1, i2 as f64))
}

fn exe_binop_i(v1: &Value, v2: &Value, arith_i: fn(i64, i64) -> i64) -> Value {
    let (i1, i2) = match (v1, v2) {
        (Value::Integer(i1), Value::Integer(i2)) => (*i1, *i2),
        (Value::Integer(i1), Value::Float(f2)) => (*i1, ftoi(*f2).unwrap()),
        (Value::Float(f1), Value::Float(f2)) => (ftoi(*f1).unwrap(), ftoi(*f2).unwrap()),
        (Value::Float(f1), Value::Integer(i2)) => (ftoi(*f1).unwrap(), *i2),
        (_, _) => todo!("meta"),
    };
    Value::Integer(arith_i(i1, i2))
}
fn exe_binop_int_i(v1: &Value, i2: u8, arith_i: fn(i64, i64) -> i64) -> Value {
    let i1 = match v1 {
        Value::Integer(i1) => *i1,
        Value::Float(f1) => ftoi(*f1).unwrap(),
        _ => todo!("meta"),
    };
    Value::Integer(arith_i(i1, i2 as i64))
}

fn exe_concat(v1: &Value, v2: &Value) -> Value {
    // TODO remove duplicated code
    let mut numbuf1: Vec<u8> = Vec::new();
    let v1 = match v1 {
        Value::Integer(i) => {
            write!(&mut numbuf1, "{}", i).unwrap();
            numbuf1.as_slice()
        }
        Value::Float(f) => {
            write!(&mut numbuf1, "{}", f).unwrap();
            numbuf1.as_slice()
        }
        _ => v1.into(),
    };

    let mut numbuf2: Vec<u8> = Vec::new();
    let v2 = match v2 {
        Value::Integer(i) => {
            write!(&mut numbuf2, "{}", i).unwrap();
            numbuf2.as_slice()
        }
        Value::Float(f) => {
            write!(&mut numbuf2, "{}", f).unwrap();
            numbuf2.as_slice()
        }
        _ => v2.into(),
    };

    [v1, v2].concat().into()
}

fn for_check<T: PartialOrd>(i: T, limit: T, is_step_positive: bool) -> bool {
    if is_step_positive {
        i <= limit
    } else {
        i >= limit
    }
}

fn for_int_limit(limit: f64, is_step_positive: bool, i: &mut i64) -> i64 {
    if is_step_positive {
        if limit < i64::MIN as f64 {
            *i = 0;
            -1
        } else {
            limit.floor() as i64
        }
    } else {
        if limit > i64::MAX as f64 {
            *i = 0;
            1
        } else {
            limit.ceil() as i64
        }
    }
}
