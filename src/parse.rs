use std::io::Read;

use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    value::Value,
};

#[derive(Debug, PartialEq)]
enum ExpDesc {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Local(usize),  // on stack, including local and temprary variables
    Global(usize), // global variable
    IndexField(usize, usize),
    IndexInt(usize, u8),
    Index(usize, usize),
}

enum ConstStack {
    Const(usize),
    Stack(usize),
}

#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,
    pub locals: Vec<String>,
    pub lex: Lex<R>,
    pub sp: usize,
}

impl<R: Read> ParseProto<R> {
    fn prefixexp(&mut self, ahead: Token) -> ExpDesc {
        let sp0 = self.sp;

        let mut desc = match ahead {
            Token::Name(var) => self.simple_name(&var),
            Token::ParL => {
                let exp = self.exp();
                self.lex.expect(Token::ParR);
                exp
            }
            t => panic!("invalid prefixexp: {:?}", t),
        };

        loop {
            match self.lex.peek() {
                Token::SqurL => {
                    self.lex.next();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = match self.exp() {
                        ExpDesc::String(s) => ExpDesc::IndexField(itable, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => {
                            ExpDesc::IndexInt(itable, u8::try_from(i).unwrap())
                        }
                        key => ExpDesc::Index(itable, self.discharge_top(key)),
                    };
                }
                Token::Dot => {
                    self.lex.next();
                    let name = self.read_name();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = ExpDesc::IndexField(itable, self.add_const(name));
                }
                Token::Colon => todo!(": Name args"),
                Token::ParL | Token::CurlyL | Token::String(_) => {
                    // args
                    self.discharge(sp0, desc);
                    desc = self.args();
                }
                _ => {
                    // Epsilon
                    return desc;
                }
            }
        }
    }

    fn args(&mut self) -> ExpDesc {
        todo!()
    }

    fn exp(&mut self) -> ExpDesc {
        let ahead = self.lex.next();
        self.exp_with_ahead(ahead)
    }

    fn exp_with_ahead(&mut self, ahead: Token) -> ExpDesc {
        match ahead {
            Token::Nil => ExpDesc::Nil,
            Token::True => ExpDesc::Boolean(true),
            Token::False => ExpDesc::Boolean(false),
            Token::Integer(i) => ExpDesc::Integer(i),
            Token::Float(f) => ExpDesc::Float(f),
            Token::String(s) => ExpDesc::String(s),
            Token::Name(var) => self.simple_name(&var),
            Token::CurlyL => self.table_constructor(),
            t => panic!("invalid exp: {:?}", t),
        }
    }

    fn simple_name(&mut self, name: &str) -> ExpDesc {
        // search reversely, so new variable covers old one with same name
        if let Some(ilocal) = self.locals.iter().rposition(|v| v == name) {
            ExpDesc::Local(ilocal)
        } else {
            ExpDesc::Global(self.add_const(name))
        }
    }

    fn discharge(&mut self, dst: usize, desc: ExpDesc) {
        let code = match desc {
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8),
            ExpDesc::Boolean(b) => ByteCode::LoadBool(dst as u8, b),
            ExpDesc::Integer(i) => {
                if let Ok(i) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, i)
                } else {
                    self.load_const(dst, i)
                }
            }
            ExpDesc::Float(f) => self.load_const(dst, f),
            ExpDesc::String(s) => self.load_const(dst, s),
            ExpDesc::Local(src) => {
                if dst != src {
                    ByteCode::Move(dst as u8, src as u8)
                } else {
                    return;
                }
            }
            ExpDesc::Global(i) => ByteCode::GetGlobal(dst as u8, i as u8),
            ExpDesc::IndexField(itable, ikey) => ByteCode::GetField(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),
        };
        self.byte_codes.push(code);
        self.sp = dst + 1;
    }

    // discharge @desc into the top of stack, if need
    fn discharge_top(&mut self, desc: ExpDesc) -> usize {
        self.discharge_if_need(self.sp, desc)
    }

    // discharge @desc into @dst, if need
    fn discharge_if_need(&mut self, dst: usize, desc: ExpDesc) -> usize {
        if let ExpDesc::Local(i) = desc {
            i // no need
        } else {
            self.discharge(dst, desc);
            dst
        }
    }

    // for constant types, add @desc to constants;
    // otherwise, discharge @desc into the top of stack
    fn discharge_const(&mut self, desc: ExpDesc) -> ConstStack {
        match desc {
            // add const
            ExpDesc::Nil => ConstStack::Const(self.add_const(())),
            ExpDesc::Boolean(b) => ConstStack::Const(self.add_const(b)),
            ExpDesc::Integer(i) => ConstStack::Const(self.add_const(i)),
            ExpDesc::Float(f) => ConstStack::Const(self.add_const(f)),
            ExpDesc::String(s) => ConstStack::Const(self.add_const(s)),

            // discharge to stack
            _ => ConstStack::Stack(self.discharge_top(desc)),
        }
    }

    fn load_exp(&mut self) {
        let sp0 = self.sp;
        let desc = self.exp();
        self.discharge(sp0, desc);
    }

    pub fn load(input: R) -> Self {
        let mut proto = ParseProto {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            locals: Vec::new(),
            lex: Lex::new(input),
            sp: 0,
        };

        proto.chunk();

        println!("constants: {:?}", &proto.constants);
        println!("byte_codes:");
        for c in proto.byte_codes.iter() {
            println!("	{:?}", c);
        }

        proto
    }

    fn chunk(&mut self) {
        loop {
            match self.lex.next() {
                Token::Name(name) => {
                    if self.lex.peek() == &Token::Assign {
                        self.assign(name);
                    } else {
                        self.func_call(name);
                    }
                }
                Token::Local => self.local(),
                Token::Eos => break,
                t => panic!("unexpected token: {t:?}"),
            }
        }
    }

    fn assign(&mut self, var: String) {
        self.lex.next();

        if let Some(i) = self.get_local(&var) {
            // local =
            self.load_expr(i);
        } else {
            // global =
            let dst = self.add_const(var) as u8;

            let code = match self.lex.next() {
                // from const values
                Token::Nil => ByteCode::SetGlobalConst(dst, self.add_const(Value::Nil) as u8),
                Token::True => ByteCode::SetGlobalConst(dst, self.add_const(true) as u8),
                Token::False => ByteCode::SetGlobalConst(dst, self.add_const(false) as u8),
                Token::Integer(i) => ByteCode::SetGlobalConst(dst, self.add_const(i) as u8),
                Token::Float(f) => ByteCode::SetGlobalConst(dst, self.add_const(f) as u8),
                Token::String(s) => ByteCode::SetGlobalConst(dst, self.add_const(s) as u8),

                // from variable
                Token::Name(var) => {
                    if let Some(src) = self.get_local(&var) {
                        // local variable
                        ByteCode::SetGlobal(dst, src as u8)
                    } else {
                        // global variable
                        ByteCode::SetGlobalGlobal(dst, self.add_const(var) as u8)
                    }
                }

                _ => panic!("invalid argument"),
            };

            self.byte_codes.push(code);
        }
    }

    fn func_call(&mut self, name: String) {
        let ifunc = self.locals.len();
        let iarg = ifunc + 1;
        // function, variable
        let code = self.load_var(ifunc, name);
        self.byte_codes.push(code);
        match self.lex.next() {
            Token::ParL => {
                self.load_expr(iarg);

                if self.lex.next() != Token::ParR {
                    panic!("expected `)`");
                }
            }
            Token::String(s) => {
                let code = self.load_const(iarg, s);
                self.byte_codes.push(code);
            }
            _ => panic!("expected string"),
        }
        self.byte_codes.push(ByteCode::Call(ifunc as u8, 1))
    }

    fn local(&mut self) {
        let var = if let Token::Name(var) = self.lex.next() {
            var
        } else {
            panic!("expected variable")
        };
        if self.lex.next() != Token::Assign {
            panic!("expected '='")
        }
        let dst: usize = self.locals.len();
        self.load_expr(dst);
        self.locals.push(var);
    }

    fn table_constructor(&mut self) -> ExpDesc {
        let table = self.sp;
        self.sp += 1;
        let inew = self.byte_codes.len();
        self.byte_codes.push(ByteCode::NewTable(table as u8, 0, 0));

        enum TableEntry {
            Map(
                (
                    fn(u8, u8, u8) -> ByteCode,
                    fn(u8, u8, u8) -> ByteCode,
                    usize,
                ),
            ),
            Array(ExpDesc),
        }

        let mut narray = 0;
        let mut nmap = 0;
        loop {
            let sp0 = self.sp;

            let entry = match self.lex.peek() {
                Token::CurlyR => {
                    self.lex.next();
                    break;
                }
                Token::SqurL => {
                    // `[` exp `]` `=` exp，通用式
                    self.lex.next();

                    let key = self.exp();
                    self.lex.expect(Token::SqurR);
                    self.lex.expect(Token::Assign);
                    TableEntry::Map(match key {
                        ExpDesc::Local(i) =>
                        // 栈上变量
                        {
                            (ByteCode::SetTable, ByteCode::SetTableConst, i)
                        }
                        ExpDesc::String(s) =>
                        // 字符串常量
                        {
                            (
                                ByteCode::SetField,
                                ByteCode::SetFieldConst,
                                self.add_const(s),
                            )
                        }
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() =>
                        // 小整数
                        {
                            (ByteCode::SetInt, ByteCode::SetIntConst, i as usize)
                        }
                        ExpDesc::Nil => panic!("nil can not be table key"),
                        ExpDesc::Float(f) if f.is_nan() => panic!("NaN can not be table key"),
                        _ =>
                        // 其他类型，则统一discharge到栈上，转变为栈上变量
                        {
                            (
                                ByteCode::SetTable,
                                ByteCode::SetTableConst,
                                self.discharge_top(key),
                            )
                        }
                    })
                }
                Token::Name(_) => {
                    let name = self.read_name();
                    if self.lex.peek() == &Token::Assign {
                        // Name `=` exp
                        self.lex.next();
                        TableEntry::Map((
                            ByteCode::SetField,
                            ByteCode::SetFieldConst,
                            self.add_const(name),
                        ))
                    } else {
                        // Name
                        TableEntry::Array(self.exp_with_ahead(Token::Name(name)))
                    }
                }
                _ => {
                    // exp
                    TableEntry::Array(self.exp())
                }
            };

            match entry {
                TableEntry::Map((op, opk, key)) => {
                    let value = self.exp(); // 读取value
                    let code = match self.discharge_const(value) {
                        // value是常量，则使用opk，如`ByteCode::SetTableConst`
                        ConstStack::Const(i) => opk(table as u8, key as u8, i as u8),

                        // value不是常量，则discharge到栈上，并使用op，如`ByteCode::SetTable`
                        ConstStack::Stack(i) => op(table as u8, key as u8, i as u8),
                    };
                    self.byte_codes.push(code);

                    nmap += 1;
                    self.sp = sp0;
                }

                TableEntry::Array(desc) => {
                    self.discharge(sp0, desc);

                    narray += 1;
                    if narray % 2 == 50 {
                        // reset the array members every 50
                        self.byte_codes.push(ByteCode::SetList(table as u8, 50));
                        self.sp = table + 1;
                    }
                }
            }

            match self.lex.next() {
                Token::SemiColon | Token::Comma => (),
                Token::CurlyR => break,
                t => panic!("invalid table {t:?}"),
            }
        }
        if self.sp > table + 1 {
            self.byte_codes.push(ByteCode::SetList(
                table as u8,
                (self.sp - (table + 1)) as u8,
            ));
        }

        self.byte_codes[inew] = ByteCode::NewTable(table as u8, narray, nmap);
        self.sp = table + 1;
        ExpDesc::Local(table)
    }

    fn read_name(&mut self) -> String {
        if let Token::Name(name) = self.lex.next() {
            name
        } else {
            panic!("expect name");
        }
    }

    fn load_expr(&mut self, dst: usize) {
        let code = match self.lex.next() {
            Token::Nil => ByteCode::LoadNil(dst as u8),
            Token::True => ByteCode::LoadBool(dst as u8, true),
            Token::False => ByteCode::LoadBool(dst as u8, false),
            Token::Integer(i) => {
                if let Ok(ii) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, ii)
                } else {
                    self.load_const(dst, Value::Integer(i))
                }
            }
            Token::Float(f) => self.load_const(dst, Value::Float(f)),
            Token::String(s) => self.load_const(dst, s),
            Token::Name(var) => self.load_var(dst, var),
            _ => panic!("invalid argument"),
        };

        self.byte_codes.push(code)
    }

    fn load_var(&mut self, dst: usize, name: String) -> ByteCode {
        if let Some(src) = self.get_local(&name) {
            ByteCode::Move(dst as u8, src as u8)
        } else {
            let ic = self.add_const(name);
            ByteCode::GetGlobal(dst as u8, ic as u8)
        }
    }

    fn get_local(&mut self, name: &String) -> Option<usize> {
        self.locals.iter().rposition(|v| *v == *name)
    }

    fn load_const(&mut self, dst: usize, c: impl Into<Value>) -> ByteCode {
        ByteCode::LoadConst(dst as u8, self.add_const(c) as u16)
    }

    fn add_const(&mut self, c: impl Into<Value>) -> usize {
        let c = c.into();
        self.constants
            .iter()
            .position(|v| *v == c)
            .unwrap_or_else(|| {
                self.constants.push(c);
                self.constants.len() - 1
            })
    }
}
