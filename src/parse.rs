use std::{cmp::Ordering, collections::btree_map::Keys, io::Read, rc::Rc};

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

    Local(usize), // on stack, including local and temprary variables
    Upvalue(usize),
    Global(usize), // global variable

    IndexField(usize, usize),
    IndexInt(usize, u8),
    Index(usize, usize),
    // IndexUpField(usize, usize),
    Function(usize),
    Closure(usize),
    Call(usize, usize),
    Vargs,

    UnaryOp(fn(u8, u8) -> ByteCode, usize),
    BinaryOp(fn(u8, u8, u8) -> ByteCode, usize, usize),
}

enum ConstStack {
    Const(usize),
    Stack(usize),
}

#[derive(Debug)]
struct GotoLabel {
    name: String,
    icode: usize,
    nvar: usize,
}

#[derive(Debug, Default)]
pub struct FuncProto {
    pub has_vargs: bool,
    pub nparam: usize,
    pub constants: Vec<Value>,
    pub byte_codes: Vec<ByteCode>,
    pub upindexes: Vec<UpIndex>,
}

#[derive(Debug, Default)]
struct Level {
    locals: Vec<(String, bool)>,
    upvalues: Vec<(String, UpIndex)>,
}

#[derive(Debug)]
struct ParseContext<R: Read> {
    levels: Vec<Level>,
    lex: Lex<R>,
}

#[derive(Debug)]
pub enum UpIndex {
    Local(usize),
    Upvalue(usize),
}

#[derive(Debug)]
pub struct ParseProto<'a, R: Read> {
    pub fp: FuncProto,

    pub sp: usize,
    pub ctx: &'a mut ParseContext<R>,
    pub break_blocks: Vec<Vec<usize>>,
    pub continue_blocks: Vec<Vec<(usize, usize)>>,
    pub gotos: Vec<GotoLabel>,
    pub labels: Vec<GotoLabel>,
}

impl<'a, R: Read> ParseProto<'a, R> {
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
        let nvar = self.local_num();
        let end_token = self.block_scope();
        self.local_expire(nvar);
        end_token
    }

    fn block_scope(&mut self) -> Token {
        let igoto = self.gotos.len();
        let ilabel = self.labels.len();
        loop {
            self.sp = self.local_num();

            match self.ctx.lex.next() {
                Token::SemiColon => (),

                t @ Token::Name(_) | t @ Token::ParL => {
                    if self.try_continue_stat(&t) {
                        continue;
                    }

                    let desc = self.prefixexp(t);
                    if let ExpDesc::Call(ifunc, narg_plus) = desc {
                        self.fp
                            .byte_codes
                            .push(ByteCode::Call(ifunc as u8, narg_plus as u8, 0));
                    } else {
                        self.assign(desc);
                    }
                }
                Token::Local => {
                    if *self.ctx.lex.peek() == Token::Function {
                        self.local_function()
                    } else {
                        self.local_variables()
                    }
                }
                Token::Function => self.function_stat(),
                Token::Return => self.ret_stat(),

                Token::If => self.if_stat(),
                Token::While => self.while_stat(),
                Token::For => self.for_stat(),
                Token::Do => self.do_stat(),
                Token::Repeat => self.repeat_stat(),
                Token::Break => self.break_stat(),
                Token::Goto => self.goto_stat(),
                Token::DoubColon => self.label_stat(),
                t => {
                    self.close_goto_labels(igoto, ilabel);
                    break t;
                }
            }
        }
    }

    // BNF:
    //   local attnamelist [`=` explist]
    //   attnamelist ::=  Name attrib {`,` Name attrib}
    fn local_variables(&mut self) {
        let mut vars = vec![self.read_name()];
        while *self.ctx.lex.peek() == Token::Comma {
            self.ctx.lex.next();
            vars.push(self.read_name())
        }

        if *self.ctx.lex.peek() == Token::Assign {
            self.ctx.lex.next();

            let want = vars.len();
            let (nexp, last_exp) = self.explist();
            match (nexp + 1).cmp(&want) {
                Ordering::Equal => self.discharge(self.sp, last_exp),
                Ordering::Less => self.discharge_expand_want(last_exp, want - nexp),
                Ordering::Greater => self.sp -= nexp - want,
            }
        } else {
            self.fp
                .byte_codes
                .push(ByteCode::LoadNil(self.sp as u8, vars.len() as u8))
        }

        // append vars into self.locals after evaluating explist
        for var in vars.into_iter() {
            self.local_new(var);
        }
    }

    // BNF:
    //   local function Name funcbody
    fn local_function(&mut self) {
        self.ctx.lex.next();

        let name = self.read_name();
        println!("== function: {name}");

        self.local_new(name);

        let f = self.funcbody(false);
        self.discharge(self.sp, f);
    }

    // BNF:
    //   function funcname funcbody
    //   funcname = Name {`.` Name} [`:` Name]
    fn function_stat(&mut self) {
        let name = self.read_name();
        let mut desc = self.simple_name(&name);

        let with_self = loop {
            match self.ctx.lex.peek() {
                Token::Dot => {
                    self.ctx.lex.next();
                    let name = self.read_name();
                    let t = self.discharge_top(desc);
                    desc = ExpDesc::IndexField(t, self.add_const(name));
                }
                Token::Colon => {
                    self.ctx.lex.next();
                    let name = self.read_name();
                    let t = self.discharge_top(desc);
                    desc = ExpDesc::IndexField(t, self.add_const(name));

                    break true;
                }
                _ => break false,
            }
        };

        let body = self.funcbody(with_self);
        self.assign_var(desc, body)
    }

    // BNF:
    //   funcbody ::= `(` [parlist] `)` block end
    //   parlist ::= namelist [`,` `...`] | `...`
    //   namelist ::= Name {`,` Name}
    fn funcbody(&mut self, with_self: bool) -> ExpDesc {
        let mut has_vargs = false;
        let mut params = Vec::new();
        if with_self {
            params.push(String::from("self"));
        }
        self.ctx.lex.expect(Token::ParL);
        loop {
            match self.ctx.lex.next() {
                Token::Name(name) => {
                    params.push(name);
                    match self.ctx.lex.next() {
                        Token::Comma => (),
                        Token::ParR => break,
                        t => panic!("invalid parameter {t:?}"),
                    }
                }
                Token::Dots => {
                    has_vargs = true;
                    self.ctx.lex.expect(Token::ParR);
                    break;
                }
                Token::ParR => break,
                t => panic!("invalid parameter {t:?}"),
            }
        }

        let proto = chunk(self.ctx, has_vargs, params, Token::End);
        
        let no_upvalue = proto.upindexes.is_empty();
        let iconst = self.add_const(Value::LuaFunction(Rc::new(proto)));
        if no_upvalue {
            ExpDesc::Function(iconst)
        } else {
            ExpDesc::Closure(iconst)
        }
    }

    // BNF:
    //   retstat ::= return [explist] [‘;’]
    fn ret_stat(&mut self) {
        let code = match self.ctx.lex.peek() {
            Token::SemiColon => {
                self.ctx.lex.next();
                ByteCode::Return0
            }
            t if is_block_end(t) => ByteCode::Return0,
            _ => {
                let iret = self.sp;
                let (nexp, last_exp) = self.explist();

                if *self.ctx.lex.peek() == Token::SemiColon {
                    self.ctx.lex.next();
                }

                if !is_block_end(self.ctx.lex.peek()) {
                    panic!("'end' expected");
                }

                if let (0, &ExpDesc::Local(i)) = (nexp, &last_exp) {
                    // only 1 return value, so NOT need discharging all values to
                    // stack top for continuity
                    ByteCode::Return(i as u8, 1)
                } else if let (0, &ExpDesc::Call(func, narg_plus)) = (nexp, &last_exp) {
                    // tail call
                    ByteCode::TailCall(func as u8, narg_plus as u8)
                } else if self.discharge_expand(last_exp) {
                    // return variable values
                    ByteCode::Return(iret as u8, 0)
                } else {
                    // return fixed values
                    ByteCode::Return(iret as u8, nexp as u8 + 1)
                }
            }
        };
        self.fp.byte_codes.push(code)
    }

    // BNF:
    //   varlist = explist
    //   varlist ::= var {`,` var}
    fn assign(&mut self, first_var: ExpDesc) {
        let mut vars = vec![first_var];
        loop {
            match self.ctx.lex.next() {
                Token::Comma => {
                    let token = self.ctx.lex.next();
                    vars.push(self.prefixexp(token));
                }
                Token::Assign => break,
                t => panic!("invalid assign: {:?}", t),
            }
        }

        let sp0 = self.sp;
        let (mut nexp, last_exp) = self.explist();

        match (nexp + 1).cmp(&vars.len()) {
            Ordering::Equal => {
                let last_var = vars.pop().unwrap();
                self.assign_var(last_var, last_exp);
            }
            Ordering::Less => {
                self.discharge_expand_want(last_exp, vars.len() - nexp);
                nexp = vars.len();
            }
            Ordering::Greater => {
                nexp = vars.len();
            }
        }

        while let Some(var) = vars.pop() {
            nexp -= 1;
            self.assign_from_stack(var, sp0 + nexp);
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
            ExpDesc::Upvalue(i) => ByteCode::SetUpvalue(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobal(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTable(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetField(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetInt(t as u8, key, value as u8),
            _ => panic!("assign from stack"),
        };

        self.fp.byte_codes.push(code);
    }

    fn assign_from_const(&mut self, var: ExpDesc, value: usize) {
        let code = match var {
            ExpDesc::Upvalue(i) => ByteCode::SetUpvalueConst(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobalConst(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTableConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetFieldConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetIntConst(t as u8, key, value as u8),
            _ => panic!("assign from stack"),
        };

        self.fp.byte_codes.push(code);
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

        let iend = self.fp.byte_codes.len() - 1;
        for i in jmp_ends.into_iter() {
            self.fp.byte_codes[i] = ByteCode::Jump((iend - i) as i16)
        }
    }

    fn do_if_block(&mut self, jmp_ends: &mut Vec<usize>) -> Token {
        let icond = self.exp_discharge_any();
        self.ctx.lex.expect(Token::Then);

        self.fp.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.fp.byte_codes.len() - 1;

        let end_token = self.block();

        if matches!(end_token, Token::Elseif | Token::Else) {
            self.fp.byte_codes.push(ByteCode::Jump(0));
            jmp_ends.push(self.fp.byte_codes.len() - 1);
        }

        let iend = self.fp.byte_codes.len() - 1;
        self.fp.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - itest) as i16);

        end_token
    }

    // BNF:
    //   while exp do block end
    fn while_stat(&mut self) {
        let istart = self.fp.byte_codes.len();

        let icond = self.exp_discharge_any();
        self.ctx.lex.expect(Token::Do);

        self.fp.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.fp.byte_codes.len() - 1;

        self.push_loop_block();

        assert_eq!(self.block(), Token::End);

        let iend = self.fp.byte_codes.len();
        self.fp
            .byte_codes
            .push(ByteCode::Jump(-((iend - istart) as i16) - 1));

        self.pop_loop_block(istart);

        self.fp.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - istart) as i16)
    }

    // * numerical: for Name `=` ...
    // * generic:   for Name {, Name} in ...
    fn for_stat(&mut self) {
        let name = self.read_name();
        if *self.ctx.lex.peek() == Token::Assign {
            self.for_numerical(name);
        } else {
            todo!("generic for")
        }
    }

    // BNF:
    //   for Name `=` exp `,` exp [`,` exp] do block end
    fn for_numerical(&mut self, name: String) {
        self.ctx.lex.next();

        let (nexp, last_exp) = self.explist();
        self.discharge(self.sp, last_exp);

        match nexp + 1 {
            2 => self.discharge(self.sp, ExpDesc::Integer(1)),
            3 => (),
            _ => panic!("invalid numerical for exp"),
        }

        self.local_new(name);
        self.local_new(String::from(""));
        self.local_new(String::from(""));

        self.ctx.lex.expect(Token::Do);

        self.fp.byte_codes.push(ByteCode::ForPrepare(0, 0));
        let iprepare = self.fp.byte_codes.len() - 1;
        let iname = self.sp - 3;

        self.push_loop_block();

        assert_eq!(self.block(), Token::End);

        self.local_expire(self.local_num() - 3);

        let d = self.fp.byte_codes.len() - iprepare;
        self.fp
            .byte_codes
            .push(ByteCode::ForLoop(iname as u8, d as u16));
        self.fp.byte_codes[iprepare] = ByteCode::ForPrepare(iname as u8, d as u16);

        self.pop_loop_block(self.fp.byte_codes.len() - 1);
    }

    // BNF:
    //   do block end
    fn do_stat(&mut self) {
        assert_eq!(self.block(), Token::End)
    }

    // BNF:
    //   repeat block until exp
    fn repeat_stat(&mut self) {
        let istart = self.fp.byte_codes.len();

        self.push_loop_block();

        let nvar = self.local_num();

        assert_eq!(self.block_scope(), Token::Until);
        let iend1 = self.fp.byte_codes.len();

        let icond = self.exp_discharge_any();

        let iend2 = self.fp.byte_codes.len();
        self.fp
            .byte_codes
            .push(ByteCode::Test(icond as u8, -((iend2 - istart + 1) as i16)));

        self.pop_loop_block(iend1);
        self.local_expire(nvar);
    }

    fn break_stat(&mut self) {
        if let Some(breaks) = self.break_blocks.last_mut() {
            self.fp.byte_codes.push(ByteCode::Jump(0));
            breaks.push(self.fp.byte_codes.len() - 1);
        } else {
            panic!("break outside loop")
        }
    }

    // BNF:
    //   goto Name
    fn goto_stat(&mut self) {
        let name = self.read_name();

        self.fp.byte_codes.push(ByteCode::Jump(0));

        self.gotos.push(GotoLabel {
            name,
            icode: self.fp.byte_codes.len() - 1,
            nvar: self.local_num(),
        })
    }

    // BNF:
    //   label ::= `::` Name `::`
    fn label_stat(&mut self) {
        let name = self.read_name();
        self.ctx.lex.expect(Token::DoubColon);

        if self.labels.iter().any(|label| label.name == name) {
            panic!("duplicate label {name}");
        }

        self.labels.push(GotoLabel {
            name,
            icode: self.fp.byte_codes.len(),
            nvar: self.local_num(),
        })
    }

    fn close_goto_labels(&mut self, igoto: usize, ilabel: usize) {
        let mut no_dsts = Vec::new();
        for goto in self.gotos.drain(igoto..) {
            if let Some(label) = self
                .labels
                .iter()
                .rev()
                .find(|label| label.name == goto.name)
            {
                if label.icode != self.fp.byte_codes.len() && label.nvar > goto.nvar {
                    panic!("goto jump into scope {}", goto.name);
                }
                let d = (label.icode as isize - goto.icode as isize) as i16;
                self.fp.byte_codes[goto.icode] = ByteCode::Jump(d - 1);
            } else {
                no_dsts.push(goto);
            }
        }
        self.gotos.append(&mut no_dsts);

        self.labels.truncate(ilabel);
    }

    fn try_continue_stat(&mut self, name: &Token) -> bool {
        if let Token::Name(name) = name {
            if name.as_str() != "continue" {
                return false;
            }
            if !matches!(
                self.ctx.lex.peek(),
                Token::End | Token::Elseif | Token::Else
            ) {
                return false;
            }

            let nvar = self.local_num();
            if let Some(continues) = self.continue_blocks.last_mut() {
                self.fp.byte_codes.push(ByteCode::Jump(0));
                continues.push((self.fp.byte_codes.len() - 1, nvar))
            } else {
                panic!("continue outside loop")
            }

            true
        } else {
            false
        }
    }

    fn push_loop_block(&mut self) {
        self.break_blocks.push(Vec::new());
        self.continue_blocks.push(Vec::new());
    }

    fn pop_loop_block(&mut self, icontinue: usize) {
        let iend = self.fp.byte_codes.len() - 1;
        for i in self.break_blocks.pop().unwrap().into_iter() {
            self.fp.byte_codes[i] = ByteCode::Jump((iend - i) as i16);
        }

        let end_nvar = self.local_num();
        for (i, i_nvar) in self.continue_blocks.pop().unwrap().into_iter() {
            if i_nvar < end_nvar {
                panic!("continue jump into local scope")
            }
            self.fp.byte_codes[i] = ByteCode::Jump((icontinue as isize - i as isize - 1) as i16)
        }
    }

    // explist ::= exp {`,` exp}
    fn explist(&mut self) -> (usize, ExpDesc) {
        let mut n = 0;
        let sp0 = self.sp;
        loop {
            let desc = self.exp();
            if *self.ctx.lex.peek() != Token::Comma {
                self.sp = sp0 + n;
                return (n, desc);
            }
            self.ctx.lex.next();

            self.discharge(sp0 + n, desc);
            n += 1;
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
        let ahead = self.ctx.lex.next();
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

            Token::Function => self.funcbody(false),
            Token::CurlyL => self.table_constructor(),
            Token::Dots => {
                if !self.fp.has_vargs {
                    panic!("no vargs")
                }
                ExpDesc::Vargs
            }

            Token::Sub => self.unop_neg(),
            Token::Not => self.unop_not(),
            Token::BitNot => self.unop_bitnot(),
            Token::Len => self.unop_len(),

            t => self.prefixexp(t),
        };

        loop {
            let (left_pri, right_pri) = self.ctx.lex.peek().binop_pri();

            if left_pri <= limit {
                return desc;
            }

            if !matches!(
                desc,
                ExpDesc::Integer(_) | ExpDesc::Float(_) | ExpDesc::String(_)
            ) {
                desc = ExpDesc::Local(self.discharge_top(desc));
            }

            let binop = self.ctx.lex.next();
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
                self.ctx.lex.expect(Token::ParR);
                exp
            }
            t => panic!("invalid prefixexp: {:?}", t),
        };

        loop {
            match self.ctx.lex.peek() {
                Token::SqurL => {
                    self.ctx.lex.next();
                    let key = self.exp();
                    self.ctx.lex.expect(Token::SqurR);
                    desc = match (desc, key) {
                        // (ExpDesc::Upvalue(itable), ExpDesc::String(key)) => {
                        //     ExpDesc::IndexUpField(itable, self.add_const(key))
                        // }
                        (table, key) => {
                            let itable = self.discharge_if_need(sp0, table);
                            match key {
                                ExpDesc::String(s) => {
                                    ExpDesc::IndexField(itable, self.add_const(s))
                                }
                                ExpDesc::Integer(i) if u8::try_from(i).is_ok() => {
                                    ExpDesc::IndexInt(itable, u8::try_from(i).unwrap())
                                }
                                _ => ExpDesc::Index(itable, self.discharge_top(key)),
                            }
                        }
                    };
                }
                Token::Dot => {
                    self.ctx.lex.next();
                    let name = self.read_name();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = ExpDesc::IndexField(itable, self.add_const(name));
                }
                Token::Colon => {
                    // :Name args
                    self.ctx.lex.next();
                    let name = self.read_name();
                    let ikey = self.add_const(name);
                    let itable = self.discharge_if_need(sp0, desc);

                    self.fp.byte_codes.push(ByteCode::GetFieldSelf(
                        sp0 as u8,
                        itable as u8,
                        ikey as u8,
                    ));

                    self.sp = sp0 + 2;

                    desc = self.args(1);
                }
                Token::ParL | Token::CurlyL | Token::String(_) => {
                    // args
                    self.discharge(sp0, desc);
                    desc = self.args(0);
                }
                _ => {
                    // Epsilon
                    return desc;
                }
            }
        }
    }

    fn local_num(&self) -> usize {
        self.ctx.levels.last().unwrap().locals.len()
    }

    fn local_new(&mut self, name: String) {
        self.ctx
            .levels
            .last_mut()
            .unwrap()
            .locals
            .push((name, false))
    }

    fn local_expire(&mut self, from: usize) {
        let mut vars = self.ctx.levels.last_mut().unwrap().locals.drain(from..);

        if vars.any(|v| v.1) {
            self.fp.byte_codes.push(ByteCode::Close(from as u8))
        }
    }

    // args ::= `(` [explist] `)` | tableconstructor | LiteralString
    fn args(&mut self, implicit_argn: usize) -> ExpDesc {
        let ifunc = self.sp - 1 - implicit_argn;
        let narg = match self.ctx.lex.next() {
            Token::ParL => {
                if *self.ctx.lex.peek() != Token::ParR {
                    let (nexp, last_exp) = self.explist();
                    self.ctx.lex.expect(Token::ParR);
                    if self.discharge_expand(last_exp) {
                        None
                    } else {
                        Some(nexp + 1)
                    }
                } else {
                    self.ctx.lex.next();
                    Some(0)
                }
            }
            Token::CurlyL => {
                self.table_constructor();
                Some(1)
            }
            Token::String(s) => {
                self.discharge(ifunc + 1, ExpDesc::String(s));
                Some(1)
            }
            t => panic!("invalid args {t:?}"),
        };

        let narg_plus = if let Some(n) = narg {
            n + implicit_argn + 1
        } else {
            0
        };

        ExpDesc::Call(ifunc, narg_plus)
    }

    fn simple_name(&mut self, name: &str) -> ExpDesc {
        let mut level_iter = self.ctx.levels.iter_mut().rev();

        // 在当前函数的局部变量中匹配，如果找到则是局部变量；
        // 在当前函数的Upvalue列表中匹配，如果找到则是已有Upvalue；（复用Upvalue）
        let level = level_iter.next().unwrap();
        // search reversely, so new variable covers old one with same name
        if let Some(i) = level.locals.iter().rposition(|v| v.0 == name) {
            return ExpDesc::Local(i);
        }
        if let Some(i) = level.upvalues.iter().position(|v| v.0 == name) {
            return ExpDesc::Upvalue(i);
        }

        // 在更外层函数的局部变量中匹配，如果找到则在所有中间层函数中创建Upvalue，并新增Upvalue；（跨多层函数的引用）
        // 在更外层函数的Upvalue中匹配，如果找到则在所有中间层函数中创建Upvalue，并新增Upvalue；（跨多层函数的Upvalue的引用）
        for (depth, level) in level_iter.enumerate() {
            if let Some(i) = level.locals.iter().rposition(|v| v.0 == name) {
                level.locals[i].1 = true;
                return self.create_upvalue(name, UpIndex::Local(i), depth);
            }

            if let Some(i) = level.upvalues.iter().position(|v| v.0 == name) {
                return self.create_upvalue(name, UpIndex::Upvalue(i), depth);
            }
        }

        ExpDesc::Global(self.add_const(name))
    }

    fn create_upvalue(&mut self, name: &str, mut upidx: UpIndex, depth: usize) -> ExpDesc {
        let levels = &mut self.ctx.levels;
        let last = levels.len() - 1;

        for Level { upvalues, .. } in levels[last - depth..last].iter_mut() {
            upvalues.push((name.into(), upidx));
            upidx = UpIndex::Upvalue(upvalues.len() - 1);
        }

        let upvalues = &mut levels[last].upvalues;
        upvalues.push((name.into(), upidx));
        ExpDesc::Upvalue(upvalues.len() - 1)
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
            ExpDesc::Upvalue(src) => ByteCode::GetUpvalue(dst as u8, src as u8),
            ExpDesc::Global(i) => ByteCode::GetGlobal(dst as u8, i as u8),

            ExpDesc::IndexField(itable, ikey) => {
                ByteCode::GetField(dst as u8, itable as u8, ikey as u8)
            }
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),

            ExpDesc::Vargs => ByteCode::Vargs(dst as u8, 1),
            ExpDesc::Function(f) => ByteCode::LoadConst(dst as u8, f as u16),
            ExpDesc::Closure(f) => ByteCode::Closure(dst as u8, f as u16),
            ExpDesc::Call(ifunc, narg_plus) => {
                ByteCode::CallSet(dst as u8, ifunc as u8, narg_plus as u8)
            }

            ExpDesc::UnaryOp(op, i) => op(dst as u8, i as u8),
            ExpDesc::BinaryOp(op, left, right) => op(dst as u8, left as u8, right as u8),
        };
        self.fp.byte_codes.push(code);
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

    fn discharge_expand_want(&mut self, desc: ExpDesc, want: usize) {
        debug_assert!(want > 1);
        let code = match desc {
            ExpDesc::Call(ifunc, narg_plus) => {
                ByteCode::Call(ifunc as u8, narg_plus as u8, want as u8)
            }
            ExpDesc::Vargs => ByteCode::Vargs(self.sp as u8, want as u8),
            _ => {
                self.discharge(self.sp, desc);
                ByteCode::LoadNil(self.sp as u8, want as u8 - 1)
            }
        };
        self.fp.byte_codes.push(code)
    }

    fn discharge_expand(&mut self, desc: ExpDesc) -> bool {
        let code = match desc {
            ExpDesc::Call(ifunc, narg_plus) => ByteCode::Call(ifunc as u8, narg_plus as u8, 0),
            ExpDesc::Vargs => ByteCode::Vargs(self.sp as u8, 0),
            _ => {
                self.discharge(self.sp, desc);
                return false;
            }
        };

        self.fp.byte_codes.push(code);
        true
    }

    // tableconstructor ::= ‘{’ [fieldlist] ‘}’
    // fieldlist ::= field {fieldsep field} [fieldsep]
    // field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
    // fieldsep ::= ‘,’ | ‘;’
    fn table_constructor(&mut self) -> ExpDesc {
        let table = self.sp;
        self.sp += 1;
        let inew = self.fp.byte_codes.len();
        self.fp
            .byte_codes
            .push(ByteCode::NewTable(table as u8, 0, 0));

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

        let mut last_array_entry = None;

        let mut narray = 0;
        let mut nmap = 0;
        loop {
            let sp0 = self.sp;

            let entry = match self.ctx.lex.peek() {
                Token::CurlyR => {
                    self.ctx.lex.next();
                    break;
                }
                Token::SqurL => {
                    // `[` exp `]` `=` exp，通用式
                    self.ctx.lex.next();

                    let key = self.exp();
                    self.ctx.lex.expect(Token::SqurR);
                    self.ctx.lex.expect(Token::Assign);
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
                    if *self.ctx.lex.peek() == Token::Assign {
                        // Name `=` exp
                        self.ctx.lex.next();
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
                    self.fp.byte_codes.push(code);

                    nmap += 1;
                    self.sp = sp0;
                }

                TableEntry::Array(desc) => {
                    if let Some(last) = last_array_entry.replace(desc) {
                        self.discharge(sp0, last);

                        narray += 1;
                        if narray % 2 == 50 {
                            // reset the array members every 50
                            self.fp.byte_codes.push(ByteCode::SetList(table as u8, 50));
                            self.sp = table + 1;
                        }
                    }
                }
            }

            match self.ctx.lex.next() {
                Token::SemiColon | Token::Comma => (),
                Token::CurlyR => break,
                t => panic!("invalid table {t:?}"),
            }
        }

        if let Some(last) = last_array_entry {
            let num = if self.discharge_expand(last) {
                0
            } else {
                narray += 1;
                (self.sp - (table + 1)) as u8
            };
            self.fp.byte_codes.push(ByteCode::SetList(table as u8, num));
        }

        self.fp.byte_codes[inew] = ByteCode::NewTable(table as u8, narray, nmap);
        self.sp = table + 1;
        ExpDesc::Local(table)
    }

    fn read_name(&mut self) -> String {
        if let Token::Name(name) = self.ctx.lex.next() {
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
        self.fp
            .constants
            .iter()
            .position(|v| *v == c)
            .unwrap_or_else(|| {
                self.fp.constants.push(c);
                self.fp.constants.len() - 1
            })
    }
}

pub fn load(input: impl Read) -> FuncProto {
    let mut ctx = ParseContext {
        lex: Lex::new(input),
        levels: Default::default(),
    };
    chunk(&mut ctx, false, Vec::new(), Token::Eos)
}

fn chunk(
    ctx: &mut ParseContext<impl Read>,
    has_vargs: bool,
    params: Vec<String>,
    end_token: Token,
) -> FuncProto {
    let fp = FuncProto {
        has_vargs,
        nparam: params.len(),
        ..Default::default()
    };

    ctx.levels.push(Level {
        locals: params.into_iter().map(|p| (p, false)).collect(),
        upvalues: Vec::new(),
    });

    let mut proto = ParseProto {
        sp: 0,
        break_blocks: Vec::new(),
        continue_blocks: Vec::new(),
        gotos: Vec::new(),
        labels: Vec::new(),

        fp,
        ctx,
    };

    assert_eq!(proto.block(), end_token);
    if let Some(goto) = proto.gotos.first() {
        panic!("goto {} no destination", &goto.name);
    }

    // clear
    let ParseProto { mut fp, ctx, .. } = proto;

    let level = ctx.levels.pop().unwrap();
    fp.upindexes = level.upvalues.into_iter().map(|u| u.1).collect();

    fp.byte_codes.push(ByteCode::Return0);

    println!("constants: {:?}", &fp.constants);
    println!("upindexes: {:?}", &fp.upindexes);
    println!("byte_codes:");
    for (i, c) in fp.byte_codes.iter().enumerate() {
        println!("  {i}\t{c:?}");
    }

    fp
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

fn is_block_end(t: &Token) -> bool {
    matches!(
        t,
        Token::End | Token::Elseif | Token::Else | Token::Until | Token::Eos
    )
}
