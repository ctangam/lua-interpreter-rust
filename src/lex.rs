use std::io::{Bytes, Read};
use std::iter::Peekable;
use std::mem;

#[derive(Debug, PartialEq)]
pub enum Token {
    // keywords
    And,    Break,  Do,     Else,   Elseif, End,
    False,  For,    Function, Goto, If,     In,
    Local,  Nil,    Not,    Or,     Repeat, Return,
    Then,   True,   Until,  While,

 // +       -       *       /       %       ^       #
    Add,    Sub,    Mul,    Div,    Mod,    Pow,    Len,
 // &       ~       |       <<      >>      //
    BitAnd, BitNot, BitOr,  ShiftL, ShiftR, Idiv,
 // ==       ~=     <=      >=      <       >        =
    Equal,  NotEq,  LesEq,  GreEq,  Less,   Greater, Assign,
 // (       )       {       }       [       ]       ::
    ParL,   ParR,   CurlyL, CurlyR, SqurL,  SqurR,  DoubColon,
 // ;               :       ,       .       ..      ...
    SemiColon,      Colon,  Comma,  Dot,    Concat, Dots,

    // constant values
    Integer(i64),
    Float(f64),
    String(Vec<u8>),

    // name of variables or table keys
    Name(String),

    // end
    Eos,
}

#[derive(Debug)]
pub struct Lex<R: Read> {
    input: Peekable<Bytes<R>>,
    ahead: Token,
}

impl<R: Read> Lex<R> {
    pub fn new(input: R) -> Self {
        Self { input: input.bytes().peekable(), ahead: Token::Eos}
    }

    pub fn next(&mut self) -> Token {
        if self.ahead == Token::Eos {
            self.do_next()
        } else {
            mem::replace(&mut self.ahead, Token::Eos)
        }
    }

    pub fn expect(&mut self, token: Token) {
        assert_eq!(self.next(), token);
    }

    pub fn peek(&mut self) -> &Token {
        if self.ahead == Token::Eos {
            self.ahead = self.do_next();
        }
        &self.ahead
    }

    fn do_next(&mut self) -> Token {
        if let Some(byte) = self.read_byte() {
            match byte {
                b' ' | b'\r' | b'\n' | b'\t' => self.do_next(),
                b'+' => Token::Add,
                b'*' => Token::Mul,
                b'%' => Token::Mod,
                b'^' => Token::Pow,
                b'#' => Token::Len,
                b'&' => Token::BitAnd,
                b'|' => Token::BitOr,
                b'(' => Token::ParL,
                b')' => Token::ParR,
                b'{' => Token::CurlyL,
                b'}' => Token::CurlyR,
                b'[' => Token::SqurL,
                b']' => Token::SqurR,
                b';' => Token::SemiColon,
                b',' => Token::Comma,
    
                b'/' => self.check_ahead(b'/', Token::Idiv, Token::Div),
                b'=' => self.check_ahead(b'=', Token::Equal, Token::Assign),
                b'~' => self.check_ahead(b'=', Token::NotEq, Token::BitNot),
                b':' => self.check_ahead(b':', Token::DoubColon, Token::Colon),
    
                b'<' => self.check_ahead2(b'=', Token::LesEq, b'<', Token::ShiftL, Token::Less),
                b'>' => self.check_ahead2(b'=', Token::GreEq, b'>', Token::ShiftR, Token::Greater),
                
                b'-' => {
                    if self.peek_byte() == b'-' {
                        self.read_byte();
                        self.read_comment();
                        self.do_next()
                    } else {
                        Token::Sub
                    }
                }
    
                b'.' => match self.peek_byte() {
                    b'.' => {
                        self.read_byte();
                        if self.peek_byte() == b'.' {
                            self.read_byte();
                            Token::Dots
                        } else {
                            Token::Concat
                        }
                    },
                    b'0'..=b'9' => {
                        self.read_number_fraction(0)
                    },
                    _ => {
                        Token::Dot
                    },  
                },
                
                quote @(b'\'' | b'"') => self.read_string(quote),
                first @(b'a'..=b'z' | b'A'..=b'Z' | b'_') => self.read_name(first),
                num @b'0'..=b'9' => self.read_number(num),
    
                b => panic!("unexpected char: {b}"),
            }
        } else {
            Token::Eos
        }
    }

    fn read_number(&mut self, first: u8) -> Token {
        if first == b'0' {
            let second = self.peek_byte();
            if second == b'x' || second == b'X' {
                return self.read_heximal()
            }
        }

        let mut n = (first - b'0') as i64;
        loop {
            let byte = self.peek_byte();
            if let Some(d) = char::to_digit(byte as char, 10) {
                self.read_byte();
                n = n * 10 + d as i64;
            } else if byte == b'.' {
                return self.read_number_fraction(n);
            } else if byte == b'e' || byte == b'E' {
                return self.read_number_exp(n as f64);
            } else {
                break;
            }
        }

        let fch = self.peek_byte();
        if (fch as char).is_alphabetic() || fch == b'.' {
            panic!("malformat number");
        }

        Token::Integer(n)
    }

    fn read_number_fraction(&mut self, i: i64) -> Token {
        self.read_byte();

        let mut n = 0;
        let mut x = 1.0;

        loop {
            let byte = self.peek_byte();
            if let Some(d) = char::to_digit(byte as char, 10) {
                self.read_byte();
                n = n * 10 + d as i64;
                x *= 10.0;
            } else {
                break;
            }
        }
        Token::Float(i as f64 + n as f64 / x)
    }

    fn read_heximal(&mut self) -> Token{
        self.read_byte();
        todo!("lex heximal")
    }

    fn read_number_exp(&mut self, _: f64) -> Token {
        self.read_byte();
        todo!("lex exp")
    }

    fn read_name(&mut self, first: u8) -> Token {
        let mut s = String::new();
        s.push(first as char);
        loop {
            match self.peek_byte() {
                c if (c as char).is_alphanumeric() || c == b'_' => {
                    self.read_byte();
                    s.push(c as char);
                }
                _ => {
                    break;
                }
            }
        }

        match &s as &str { // TODO optimize by hash
            "and"      => Token::And,
            "break"    => Token::Break,
            "do"       => Token::Do,
            "else"     => Token::Else,
            "elseif"   => Token::Elseif,
            "end"      => Token::End,
            "false"    => Token::False,
            "for"      => Token::For,
            "function" => Token::Function,
            "goto"     => Token::Goto,
            "if"       => Token::If,
            "in"       => Token::In,
            "local"    => Token::Local,
            "nil"      => Token::Nil,
            "not"      => Token::Not,
            "or"       => Token::Or,
            "repeat"   => Token::Repeat,
            "return"   => Token::Return,
            "then"     => Token::Then,
            "true"     => Token::True,
            "until"    => Token::Until,
            "while"    => Token::While,
            _          => Token::Name(s),
        }
    }
    
    fn read_string(&mut self, quote: u8) -> Token {
        let mut s = Vec::new();
        loop {
            match self.read_byte().expect("unfinished string") {
                b'\n' => panic!("unfinished string"),
                b'\\' => s.push(self.read_escape()),
                byte if byte == quote => break,
                byte => s.push(byte),
            }
        }
        Token::String(s)
    }

    fn read_escape(&mut self) -> u8 {
        match self.read_byte().expect("string escape") {
            b'a' => 0x07,
            b'b' => 0x08,
            b'f' => 0x0c,
            b'v' => 0x0b,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'\\' => b'\\',
            b'"' => b'"',
            b'\'' => b'\'',
            b'x' => { // format: \xXX
                let n1 = char::to_digit(self.read_byte().unwrap() as char, 16).unwrap();
                let n2 = char::to_digit(self.read_byte().unwrap() as char, 16).unwrap();
                (n1 * 16 + n2) as u8
            }
            ch@b'0'..=b'9' => { // format: \d[d[d]]
                let mut n = char::to_digit(ch as char, 10).unwrap(); // TODO no unwrap
                if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                    self.read_byte();
                    n = n * 10 + d;
                    if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                        self.read_byte();
                        n = n * 10 + d;
                    }
                }
                u8::try_from(n).expect("decimal escape too large")
            }
            _ => panic!("invalid string escape")
        }
    }

    // '--' has been read
    fn read_comment(&mut self) {
        match self.read_byte() {
            None => (),
            Some(b'[') => todo!("long comment"),
            Some(_) => { // line comment
                while let Some(byt) = self.read_byte() {
                    if byt == b'\n' {
                        break;
                    }
                }
            }
        }
    }
    
    fn read_byte(&mut self) -> Option<u8> {
        self.input.next().map(|r| r.unwrap())
    }

    fn peek_byte(&mut self) -> u8 {
        match self.input.peek() {
            Some(Ok(ch)) => *ch,
            Some(_) => panic!("lex peek error"),
            None => b'\0',
        }
    }

    fn check_ahead(&mut self, ahead: u8, long: Token, short: Token) -> Token {
        if self.peek_byte() == ahead {
            self.read_byte();
            long
        } else {
            short
        }
    }

    fn check_ahead2(&mut self, ahead1: u8, long1: Token, ahead2: u8, long2: Token, short: Token) -> Token {
        let ch = self.peek_byte();
        if ch == ahead1 {
            self.read_byte();
            long1
        } else if ch == ahead2 {
            self.read_byte();
            long2
        } else {
            short
        }
    }
}

