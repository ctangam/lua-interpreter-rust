use std::{cmp::Ordering, collections::btree_map::Keys, io::Read};

use crate::{
    bytecode::ByteCode,
    lex::{Lex, Token},
    utils::ftoi,
    value::{self, Value},
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

    Call,

    UnaryOp(fn(u8, u8) -> ByteCode, usize),
    BinaryOp(fn(u8, u8, u8) -> ByteCode, usize, usize),
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
    pub break_blocks: Vec<Vec<usize>>,
    pub lex: Lex<R>,
    pub sp: usize,
}

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> Self {
        let mut proto = ParseProto {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            locals: Vec::new(),
            break_blocks: Vec::new(),
            lex: Lex::new(input),
            sp: 0,
        };
        proto.chunk();

        println!("constants: {:?}", &proto.constants);
        println!("byte_codes:");
        for (i,c) in proto.byte_codes.iter().enumerate() {
            println!("  {i}\t{c:?}");
        }
        println!("===============================");

        proto
    }

    fn chunk(&mut self) {
        assert_eq!(self.block(), Token::Eos);
    }

    // BNF:
    //   block ::= {stat} [retstat]
    //   stat ::= `;` |
    //     varlist `=` explist |
    //     functioncall |
    //     label |
    //     break |
    //     goto Name |
    //     do block end |
    //     while exp do block end |
    //     repeat block until exp |
    //     if exp then block {elseif exp then block} [else block] end |
    //     for Name `=` exp `,` exp [`,` exp] do block end |
    //     for namelist in explist do block end |
    //     function funcname funcbody |
    //     local function Name funcbody |
    //     local attnamelist [`=` explist]
    fn block(&mut self) -> Token {
        let nvar = self.locals.len();
        let end_token = self.block_scope();
        self.locals.truncate(nvar);
        end_token
    }

    fn block_scope(&mut self) -> Token {
        loop {
            self.sp = self.locals.len();
    
            match self.lex.next() {
                Token::SemiColon => (),
    
                t @ Token::Name(_) | t @ Token::ParL => {
                    let desc = self.prefixexp(t);
                    if desc == ExpDesc::Call {
                    } else {
                        self.assign(desc);
                    }
                }
                Token::Local => self.local(),
                Token::If => self.if_stat(),
                Token::While => self.while_stat(),
                t => break t,
            }
        }
    }
    
    // BNF:
    //   local attnamelist [`=` explist]
    //   attnamelist ::=  Name attrib {`,` Name attrib}
    fn local(&mut self) {
        let mut vars = Vec::new();
        let nexp = loop {
            vars.push(self.read_name());

            match self.lex.peek() {
                Token::Comma => {
                    self.lex.next();
                }
                Token::Assign => {
                    self.lex.next();
                    break self.explist();
                }
                _ => break 0,
            }
        };

        if nexp < vars.len() {
            let ivar = self.locals.len() + nexp;
            let nnil = vars.len() - nexp;
            self.byte_codes
                .push(ByteCode::LoadNil(ivar as u8, nnil as u8));
        }

        self.locals.append(&mut vars)
    }

    // BNF:
    //   varlist = explist
    //   varlist ::= var {`,` var}
    fn assign(&mut self, first_var: ExpDesc) {
        let mut vars = vec![first_var];
        loop {
            match self.lex.next() {
                Token::Comma => {
                    let token = self.lex.next();
                    vars.push(self.prefixexp(token));
                }
                Token::Assign => break,
                t => panic!("invalid assign: {:?}", t),
            }
        }

        let exp_sp0 = self.sp;
        let mut nfexp = 0;
        let last_exp = loop {
            let desc = self.exp();

            if *self.lex.peek() == Token::Comma {
                self.lex.next();
                self.discharge(exp_sp0 + nfexp, desc);
                nfexp += 1;
            } else {
                break desc;
            }
        };

        match (nfexp + 1).cmp(&vars.len()) {
            Ordering::Equal => {
                let last_var = vars.pop().unwrap();
                self.assign_var(last_var, last_exp);
            }
            Ordering::Less => todo!("expand last exps"),
            Ordering::Greater => {
                nfexp = vars.len();
            }
        }

        while let Some(var) = vars.pop() {
            nfexp -= 1;
            self.assign_from_stack(var, exp_sp0 + nfexp);
        }
    }

    fn assign_var(&mut self, var: ExpDesc, value: ExpDesc) {
        if let ExpDesc::Local(ivar) = var {
            self.discharge(ivar, value);
        } else {
            match self.discharge_const(value) {
                ConstStack::Const(i) => self.assign_from_const(var, i),
                ConstStack::Stack(i) => self.assign_from_stack(var, i),
            }
        }
    }

    fn assign_from_stack(&mut self, var: ExpDesc, value: usize) {
        let code = match var {
            ExpDesc::Local(i) => ByteCode::Move(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobal(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTable(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetField(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetInt(t as u8, key, value as u8),
            _ => panic!("assign from stack"),
        };

        self.byte_codes.push(code);
    }

    fn assign_from_const(&mut self, var: ExpDesc, value: usize) {
        let code = match var {
            ExpDesc::Global(name) => ByteCode::SetGlobalConst(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTableConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetFieldConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetIntConst(t as u8, key, value as u8),
            _ => panic!("assign from stack"),
        };

        self.byte_codes.push(code);
    }

    // BNF:
    //   if exp then block {elseif exp then block} [else block] end
    fn if_stat(&mut self) {
        let mut jmp_ends = Vec::new();

        let mut end_token = self.do_if_block(&mut jmp_ends);

        while end_token == Token::Elseif {
            end_token = self.do_if_block(&mut jmp_ends);
        }

        if end_token == Token::Else {
            end_token = self.block();
        }

        assert_eq!(end_token, Token::End);

        let iend = self.byte_codes.len() - 1;
        for i in jmp_ends.into_iter() {
            self.byte_codes[i] = ByteCode::Jump((iend - i) as i16)
        }
    }

    fn do_if_block(&mut self, jmp_ends: &mut Vec<usize>) -> Token {
        let icond = self.exp_discharge_any();
        self.lex.expect(Token::Then);

        self.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.byte_codes.len() - 1;

        let end_token = self.block();

        if matches!(end_token, Token::Elseif | Token::Else) {
            self.byte_codes.push(ByteCode::Jump(0));
            jmp_ends.push(self.byte_codes.len() - 1);
        }

        let iend = self.byte_codes.len() - 1;
        self.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - itest) as i16);

        end_token
    }

    // BNF:
    //   while exp do block end
    fn while_stat(&mut self) {
        let istart = self.byte_codes.len();

        let icond = self.exp_discharge_any();
        self.lex.expect(Token::Do);

        self.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.byte_codes.len() - 1;

        // self.push_loop_block();

        assert_eq!(self.block(), Token::End);

        let iend = self.byte_codes.len();
        self.byte_codes.push(ByteCode::Jump(-((iend - istart) as i16) - 1));

        // self.pop_loop_block(istart);

        self.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - istart) as i16)
    }

    // explist ::= exp {`,` exp}
    fn explist(&mut self) -> usize {
        let mut n = 0;
        let sp0 = self.sp;
        loop {
            let desc = self.exp();
            self.discharge(sp0 + n, desc);
            n += 1;

            if self.lex.peek() != &Token::Comma {
                return n;
            }

            self.lex.next();
        }
    }

    fn exp_discharge_any(&mut self) -> usize {
        let e = self.exp();
        self.discharge_top(e)
    }

    // BNF:
    //   exp ::= nil | false | true | Numeral | LiteralString | `...` | functiondef |
    //           prefixexp | tableconstructor | exp binop exp | unop exp
    //
    // Remove left recursion:
    //
    //   exp ::= (nil | false | true | Numeral | LiteralString | `...` | functiondef |
    //           prefixexp | tableconstructor | unop exp) A'
    // where:
    //   A' ::= binop exp A' | Epsilon
    fn exp(&mut self) -> ExpDesc {
        self.exp_limit(0)
    }

    fn exp_limit(&mut self, limit: i32) -> ExpDesc {
        let ahead = self.lex.next();
        self.do_exp(limit, ahead)
    }

    fn exp_with_ahead(&mut self, ahead: Token) -> ExpDesc {
        self.do_exp(0, ahead)
    }

    fn do_exp(&mut self, limit: i32, ahead: Token) -> ExpDesc {
        let mut desc = match ahead {
            Token::Nil => ExpDesc::Nil,
            Token::True => ExpDesc::Boolean(true),
            Token::False => ExpDesc::Boolean(false),
            Token::Integer(i) => ExpDesc::Integer(i),
            Token::Float(f) => ExpDesc::Float(f),
            Token::String(s) => ExpDesc::String(s),

            Token::Function => todo!("function"),
            Token::CurlyL => self.table_constructor(),
            Token::Dots => todo!("dots"),

            Token::Sub => self.unop_neg(),
            Token::Not => self.unop_not(),
            Token::BitNot => self.unop_bitnot(),
            Token::Len => self.unop_len(),

            t => self.prefixexp(t),
        };

        loop {
            let (left_pri, right_pri) = self.lex.peek().binop_pri();

            if left_pri <= limit {
                return desc;
            }

            if !matches!(
                desc,
                ExpDesc::Integer(_) | ExpDesc::Float(_) | ExpDesc::String(_)
            ) {
                desc = ExpDesc::Local(self.discharge_top(desc));
            }

            let binop = self.lex.next();
            let right_desc = self.exp_limit(right_pri);
            desc = self.process_binop(binop, desc, right_desc);
        }
    }

    fn exp_unop(&mut self) -> ExpDesc {
        self.exp_limit(12)
    }

    // BNF:
    //   prefixexp ::= var | functioncall | `(` exp `)`
    //   var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
    //   functioncall ::=  prefixexp args | prefixexp `:` Name args
    //
    // We need to remove left recursion amount these 3 rules.
    //
    // First unfold 'var' and 'functioncall' in 'prefixexp' to remove indirect recursion:
    //
    //   prefixexp ::= Name | prefixexp `[` exp `]` | prefixexp `.` Name | prefixexp args | prefixexp `:` Name args | `(` exp `)`
    //
    // Then remove the direct left recursion following:
    //   A ::= A alpha | beta
    // into
    //   A ::= beta A'
    //   A' ::= alpha A' | Epsilon
    //
    // so
    //   prefixexp ::= prefixexp (`[` exp `]` | `.` Name | args | `:` Name args) | Name | `(` exp `)`
    //               = prefixexp alpha | beta
    // where
    //   alpha ::= `[` exp `]` | `.` Name | args | `:` Name args
    //   beta ::= Name | `(` exp `)`
    //
    // Finally we get:
    //   prefixexp ::= beta A'
    //               = (Name | `(` exp `)`) A'
    // where:
    //   A' ::= alpha A' | Epsilon
    //        = (`[` exp `]` | `.` Name | args | `:` Name args) A' | Epsilon
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
                    self.lex.expect(Token::SqurR);
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

    // args ::= `(` [explist] `)` | tableconstructor | LiteralString
    fn args(&mut self) -> ExpDesc {
        let ifunc = self.sp - 1;
        let argn = match self.lex.next() {
            Token::ParL => {
                if *self.lex.peek() != Token::ParR {
                    let argn = self.explist();
                    self.lex.expect(Token::ParR);
                    argn
                } else {
                    self.lex.next();
                    0
                }
            }
            Token::CurlyL => {
                self.table_constructor();
                1
            }
            Token::String(s) => {
                self.discharge(ifunc + 1, ExpDesc::String(s));
                1
            }
            t => panic!("invalid args {t:?}"),
        };

        self.byte_codes
            .push(ByteCode::Call(ifunc as u8, argn as u8));
        ExpDesc::Call
    }

    fn simple_name(&mut self, name: &str) -> ExpDesc {
        // search reversely, so new variable covers old one with same name
        if let Some(ilocal) = self.locals.iter().rposition(|v| v == name) {
            ExpDesc::Local(ilocal)
        } else {
            ExpDesc::Global(self.add_const(name))
        }
    }

    fn unop_neg(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Integer(i) => ExpDesc::Integer(-i),
            ExpDesc::Float(f) => ExpDesc::Float(-f),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::String(_) => panic!("invalid - operator"),
            desc => ExpDesc::UnaryOp(ByteCode::Neg, self.discharge_top(desc)),
        }
    }

    fn unop_not(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Nil => ExpDesc::Boolean(true),
            ExpDesc::Boolean(b) => ExpDesc::Boolean(!b),
            ExpDesc::Integer(_) | ExpDesc::Float(_) | ExpDesc::String(_) => ExpDesc::Boolean(false),
            desc => ExpDesc::UnaryOp(ByteCode::Not, self.discharge_top(desc)),
        }
    }

    fn unop_bitnot(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Integer(i) => ExpDesc::Integer(!i),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::Float(_) | ExpDesc::String(_) => {
                panic!("invalid ~ operator")
            }
            desc => ExpDesc::UnaryOp(ByteCode::BitNot, self.discharge_top(desc)),
        }
    }

    fn unop_len(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::String(s) => ExpDesc::Integer(s.len() as i64),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::Integer(_) | ExpDesc::Float(_) => {
                panic!("invalid # operator")
            }
            desc => ExpDesc::UnaryOp(ByteCode::Len, self.discharge_top(desc)),
        }
    }

    fn process_binop(&mut self, binop: Token, left: ExpDesc, right: ExpDesc) -> ExpDesc {
        if let Some(r) = fold_const(&binop, &left, &right) {
            return r;
        }

        match binop {
            Token::Add => self.do_binop(
                left,
                right,
                ByteCode::Add,
                ByteCode::AddInt,
                ByteCode::AddConst,
            ),
            Token::Sub => self.do_binop(
                left,
                right,
                ByteCode::Sub,
                ByteCode::SubInt,
                ByteCode::SubConst,
            ),
            Token::Mul => self.do_binop(
                left,
                right,
                ByteCode::Mul,
                ByteCode::MulInt,
                ByteCode::MulConst,
            ),
            Token::Mod => self.do_binop(
                left,
                right,
                ByteCode::Mod,
                ByteCode::ModInt,
                ByteCode::ModConst,
            ),
            Token::Idiv => self.do_binop(
                left,
                right,
                ByteCode::Idiv,
                ByteCode::IdivInt,
                ByteCode::IdivConst,
            ),
            Token::Div => self.do_binop(
                left,
                right,
                ByteCode::Div,
                ByteCode::DivInt,
                ByteCode::DivConst,
            ),
            Token::Pow => self.do_binop(
                left,
                right,
                ByteCode::Pow,
                ByteCode::PowInt,
                ByteCode::PowConst,
            ),
            Token::BitAnd => self.do_binop(
                left,
                right,
                ByteCode::BitAnd,
                ByteCode::BitAndInt,
                ByteCode::BitAndConst,
            ),
            Token::BitNot => self.do_binop(
                left,
                right,
                ByteCode::BitXor,
                ByteCode::BitXorInt,
                ByteCode::BitXorConst,
            ),
            Token::BitOr => self.do_binop(
                left,
                right,
                ByteCode::BitOr,
                ByteCode::BitOrInt,
                ByteCode::BitOrConst,
            ),
            Token::ShiftL => self.do_binop(
                left,
                right,
                ByteCode::ShiftL,
                ByteCode::ShiftLInt,
                ByteCode::ShiftLConst,
            ),
            Token::ShiftR => self.do_binop(
                left,
                right,
                ByteCode::ShiftR,
                ByteCode::ShiftRInt,
                ByteCode::ShiftRConst,
            ),
            Token::Concat => self.do_binop(
                left,
                right,
                ByteCode::Concat,
                ByteCode::ConcatInt,
                ByteCode::ConcatConst,
            ),
            _ => panic!("impossible"),
        }
    }

    fn do_binop(
        &mut self,
        mut left: ExpDesc,
        mut right: ExpDesc,
        opr: fn(u8, u8, u8) -> ByteCode,
        opi: fn(u8, u8, u8) -> ByteCode,
        opk: fn(u8, u8, u8) -> ByteCode,
    ) -> ExpDesc {
        if opr == ByteCode::Add || opr == ByteCode::Mul {
            if matches!(left, ExpDesc::Integer(_) | ExpDesc::Float(_)) {
                (left, right) = (right, left);
            }
        }

        let left = self.discharge_top(left);

        let (op, right) = match right {
            ExpDesc::Integer(i) => {
                if let Ok(i) = u8::try_from(i) {
                    (opi, i as usize)
                } else {
                    (opk, self.add_const(i))
                }
            }
            ExpDesc::Float(f) => (opk, self.add_const(f)),
            _ => (opr, self.discharge_top(right)),
        };
        ExpDesc::BinaryOp(op, left, right)
    }

    fn discharge(&mut self, dst: usize, desc: ExpDesc) {
        let code = match desc {
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8, 1),
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

            ExpDesc::IndexField(itable, ikey) => {
                ByteCode::GetField(dst as u8, itable as u8, ikey as u8)
            }
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),

            ExpDesc::Call => todo!("discharge Call"),

            ExpDesc::UnaryOp(op, i) => op(dst as u8, i as u8),
            ExpDesc::BinaryOp(op, left, right) => op(dst as u8, left as u8, right as u8),
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

    // tableconstructor ::= ‘{’ [fieldlist] ‘}’
    // fieldlist ::= field {fieldsep field} [fieldsep]
    // field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
    // fieldsep ::= ‘,’ | ‘;’
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
                    if *self.lex.peek() == Token::Assign {
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

impl Token {
    fn binop_pri(&self) -> (i32, i32) {
        match self {
            Token::Pow => (14, 13), // right associative
            Token::Mul | Token::Mod | Token::Div | Token::Idiv => (11, 11),
            Token::Add | Token::Sub => (10, 10),
            Token::Concat => (9, 8), // right associative
            Token::ShiftL | Token::ShiftR => (7, 7),
            Token::BitAnd => (6, 6),
            Token::BitNot => (5, 5),
            Token::BitOr => (4, 4),
            Token::Equal
            | Token::NotEq
            | Token::Less
            | Token::Greater
            | Token::LesEq
            | Token::GreEq => (3, 3),
            Token::And => (2, 2),
            Token::Or => (1, 1),
            _ => (-1, -1),
        }
    }
}

fn fold_const(binop: &Token, left: &ExpDesc, right: &ExpDesc) -> Option<ExpDesc> {
    match binop {
        Token::Add => do_fold_const(left, right, |a, b| a + b, |a, b| a + b),
        Token::Sub => do_fold_const(left, right, |a, b| a - b, |a, b| a - b),
        Token::Mul => do_fold_const(left, right, |a, b| a * b, |a, b| a * b),
        Token::Mod => do_fold_const(left, right, |a, b| a % b, |a, b| a % b),
        Token::Idiv => do_fold_const(left, right, |a, b| a / b, |a, b| a / b),

        Token::Div => do_fold_const_float(left, right, |a, b| a / b),
        Token::Pow => do_fold_const_float(left, right, |a, b| a.powf(b)),

        Token::BitAnd => do_fold_const_int(left, right, |a, b| a & b),
        Token::BitNot => do_fold_const_int(left, right, |a, b| a ^ b),
        Token::BitOr => do_fold_const_int(left, right, |a, b| a | b),
        Token::ShiftL => do_fold_const_int(left, right, |a, b| a << b),
        Token::ShiftR => do_fold_const_int(left, right, |a, b| a >> b),

        Token::Concat => {
            if let (ExpDesc::String(s1), ExpDesc::String(s2)) = (left, right) {
                Some(ExpDesc::String([s1.as_slice(), s2.as_slice()].concat()))
            } else {
                None
            }
        }
        _ => panic!("impossible"),
    }
}

fn do_fold_const(
    left: &ExpDesc,
    right: &ExpDesc,
    arith_i: fn(i64, i64) -> i64,
    arith_f: fn(f64, f64) -> f64,
) -> Option<ExpDesc> {
    match (left, right) {
        (ExpDesc::Integer(i1), ExpDesc::Integer(i2)) => Some(ExpDesc::Integer(arith_i(*i1, *i2))),
        (ExpDesc::Float(f1), ExpDesc::Float(f2)) => Some(ExpDesc::Float(arith_f(*f1, *f2))),
        (ExpDesc::Float(f1), ExpDesc::Integer(i2)) => {
            Some(ExpDesc::Float(arith_f(*f1, *i2 as f64)))
        }
        (ExpDesc::Integer(i1), ExpDesc::Float(f2)) => {
            Some(ExpDesc::Float(arith_f(*i1 as f64, *f2)))
        }
        (_, _) => None,
    }
}

fn do_fold_const_int(
    left: &ExpDesc,
    right: &ExpDesc,
    arith_i: fn(i64, i64) -> i64,
) -> Option<ExpDesc> {
    let (i1, i2) = match (left, right) {
        (ExpDesc::Integer(i1), ExpDesc::Integer(i2)) => (*i1, *i2),
        (ExpDesc::Float(f1), ExpDesc::Float(f2)) => (ftoi(*f1).unwrap(), ftoi(*f2).unwrap()),
        (ExpDesc::Float(f1), ExpDesc::Integer(i2)) => (ftoi(*f1).unwrap(), *i2),
        (ExpDesc::Integer(i1), ExpDesc::Float(f2)) => (*i1, ftoi(*f2).unwrap()),
        (_, _) => return None,
    };
    Some(ExpDesc::Integer(arith_i(i1, i2)))
}

fn do_fold_const_float(
    left: &ExpDesc,
    right: &ExpDesc,
    arith_f: fn(f64, f64) -> f64,
) -> Option<ExpDesc> {
    let (f1, f2) = match (left, right) {
        (ExpDesc::Integer(i1), ExpDesc::Integer(i2)) => (*i1 as f64, *i2 as f64),
        (ExpDesc::Float(f1), ExpDesc::Float(f2)) => (*f1, *f2),
        (ExpDesc::Float(f1), ExpDesc::Integer(i2)) => (*f1, *i2 as f64),
        (ExpDesc::Integer(i1), ExpDesc::Float(f2)) => (*i1 as f64, *f2),
        (_, _) => return None,
    };
    Some(ExpDesc::Float(arith_f(f1, f2)))
}
