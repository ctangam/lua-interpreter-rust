use std::io::Read;

use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    value::Value,
};

#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,
    pub locals: Vec<String>,
    pub lex: Lex<R>,
}

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> Self {
        let mut proto = ParseProto {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            locals: Vec::new(),
            lex: Lex::new(input),
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
				 Token::Name(var) =>
					 if let Some(src) = self.get_local(&var) {
						 // local variable
						 ByteCode::SetGlobal(dst, src as u8)
					 } else {
						 // global variable
						 ByteCode::SetGlobalGlobal(dst, self.add_const(var) as u8)
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
                let code = self.load_const(iarg, s.into());
                self.byte_codes.push(code);
            }
            Token::CurlyL => self.table_constructor(iarg),
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

    fn table_constructor(&mut self, dst: usize) {
        let table = dst as u8;
        let inew = self.byte_codes.len();
        self.byte_codes.push(ByteCode::NewTable(table, 0, 0));

        let mut narray = 0;
        let mut nmap = 0;
        let mut sp = dst + 1;
        loop {
            match self.lex.peek() {
                Token::CurlyR => {
                    self.lex.next();
                    break;
                },
                Token::SqurL => { // `[` exp `]` `=` exp，通用式
                    nmap += 1;
                    self.lex.next();
                    
                    self.load_expr(sp);
                    self.lex.expect(Token::SqurR);
                    self.lex.expect(Token::Assign);
                    self.load_expr(sp + 1);

                    self.byte_codes.push(ByteCode::SetTable(table, sp as u8, sp as u8 + 1))
                },
                Token::Name(_) => { // Name `=` exp | Name
                    nmap += 1;
                    let name = if let Token::Name(name) = self.lex.next() {
                        name
                    } else {
                        unreachable!()
                    };
                    if self.lex.peek() == &Token::Assign {
                        self.lex.next();
                        let key = self.add_const(name) as u8;
                        self.load_expr(sp);
                        self.byte_codes.push(ByteCode::SetField(table, key, sp as u8));
                    } else {
                        narray += 1;
                        let code = self.load_var(sp, name);
                        self.byte_codes.push(code);
    
                        sp += 1;
                        if sp - (dst + 1) > 50 {
                            self.byte_codes.push(ByteCode::SetList(table, (sp - (dst + 1)) as u8));
                            sp = dst + 1;
                        }
                    }
                },
                _ => {
                    narray += 1;
                    self.load_expr(sp);

                    sp += 1;
                    if sp - (dst + 1) > 50 {
                        self.byte_codes.push(ByteCode::SetList(table, (sp - (dst + 1)) as u8));
                        sp = dst + 1;
                    }
                },
            }

            match self.lex.next() {
                Token::SemiColon | Token::Comma => (),
                Token::CurlyR => break,
                t => panic!("invalid table {t:?}"),
            }
        }
        if sp > dst + 1 {
            self.byte_codes.push(ByteCode::SetList(table, (sp - (dst + 1)) as u8));
        }

        self.byte_codes[inew] = ByteCode::NewTable(table, narray, nmap);
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
            Token::String(s) => self.load_const(dst, s.into()),
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
	
	fn load_const(&mut self, dst: usize, c: Value) -> ByteCode {
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
