use std::{cell::RefCell, collections::HashMap, fmt, hash::{Hash, Hasher}, mem, rc::Rc};

use crate::{parse::FuncProto, vm::ExeState};

const SHORT_STR_MAX: usize = 16 - 1 - 1;
const MID_STR_MAX: usize = 48 - 1;

pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}

impl Table {
    pub fn new(narray: usize, nmap: usize) -> Self {
        Table {
            array: Vec::with_capacity(narray),
            map: HashMap::with_capacity(nmap),
        }
    }
}

pub enum Upvalue {
    Open(usize),
    Closed(usize),
}

pub struct LuaClosure {
    proto: Rc<FuncProto>,
    upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Clone)]
pub enum Value{
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    ShortStr(u8, [u8; SHORT_STR_MAX]),
    MidStr(Rc<(u8, [u8; MID_STR_MAX])>),
    LongStr(Rc<Vec<u8>>),
    LuaFunction(Rc<FuncProto>),
    LuaClosure(Rc<LuaClosure>),
    RustFunction(fn (&mut ExeState) -> i32),
    Table(Rc<RefCell<Table>>),
}

impl Value {
    pub fn same(&self, other: &Self) -> bool {
        // eliminate Integer and Float with same number value
        mem::discriminant(self) == mem::discriminant(other) && self == other
    }
    pub fn ty(&self) -> &'static str {
        match self {
            &Value::Nil => "nil",
            &Value::Boolean(_) => "boolean",
            &Value::Integer(_) => "number",
            &Value::Float(_) => "number",
            &Value::ShortStr(_, _) => "string",
            &Value::MidStr(_) => "string",
            &Value::LongStr(_) => "string",
            &Value::Table(_) => "table",
            &Value::RustFunction(_) => "function",
            &Value::LuaFunction(_) => "function",
            &Value::LuaClosure(_) => "closure",
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::ShortStr(len, buf) => write!(f, "'{}'", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::MidStr(s) => write!(f, "\"{}\"", String::from_utf8_lossy(&s.1[..s.0 as usize])),
            Value::LongStr(s) => write!(f, "'''{}'''", String::from_utf8_lossy(s)),
            Value::Table(t) => {
                let t = t.borrow();
                write!(f, "table:{}:{}", t.array.len(), t.map.len())
            }
            Value::RustFunction(_) => write!(f, "function"),
            Value::LuaFunction(_) => write!(f, "Lua function"),
            Value::LuaClosure(_) => write!(f, "Lua closure"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Table(t) => write!(f, "table: {:?}", Rc::as_ptr(t)),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(b1), Value::Boolean(b2)) => *b1 == *b2,
            (Value::Integer(i1), Value::Integer(i2)) => *i1 == *i2,
            (&Value::Integer(i), &Value::Float(f)) |
            (&Value::Float(f), &Value::Integer(i)) => i as f64 == f && i == f as i64,
            (Value::Float(f1), Value::Float(f2)) => *f1 == *f2,
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => s1[..*len1 as usize] == s2[..*len2 as usize],
            (Value::MidStr(s1), Value::MidStr(s2)) => s1.1[..s1.0 as usize] == s2.1[..s2.0 as usize],
            (Value::LongStr(s1), Value::LongStr(s2)) => s1 == s2,
            (Value::RustFunction(f1), Value::RustFunction(f2)) => std::ptr::eq(f1, f2),
            (Value::LuaFunction(f1), Value::LuaFunction(f2)) => Rc::as_ptr(f1) == Rc::as_ptr(f2),
            (_, _) => false,
        }
    }
}

impl From<Vec<u8>> for Value {
    fn from(s: Vec<u8>) -> Self {
        vec_to_short_mid_str(&s).unwrap_or(Value::LongStr(Rc::new(s)))
    }
}

impl From<&[u8]> for Value {
    fn from(s: &[u8]) -> Self {
        vec_to_short_mid_str(s).unwrap_or(Value::LongStr(Rc::new(s.to_vec())))
    }
}

fn vec_to_short_mid_str(v: &[u8]) -> Option<Value> {
    let len = v.len();
    if len <= SHORT_STR_MAX {
        let mut buf = [0; SHORT_STR_MAX];
        buf[..len].copy_from_slice(v);
        Some(Value::ShortStr(len as u8, buf))
    } else if len <= MID_STR_MAX {
        let mut buf = [0; MID_STR_MAX];
        buf[..len].copy_from_slice(v);
        Some(Value::MidStr(Rc::new((len as u8, buf))))
    } else {
        None
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        s.into_bytes().into()
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        s.as_bytes().into()
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}

impl<'a> From<&'a Value> for &'a [u8] {
    fn from(value: &'a Value) -> Self {
        match value {
            Value::ShortStr(len, buf) => &buf[..*len as usize],
            Value::MidStr(s) => &s.1[..s.0 as usize],
            Value::LongStr(s) => s,
            _ => panic!("invalid string value: {value:?}"),
        }
    }
}

impl<'a> From<&'a Value> for &'a str {
    fn from(value: &'a Value) -> Self {
        std::str::from_utf8(value.into()).unwrap()
    }
}

impl From<&Value> for String {
    fn from(value: &Value) -> Self {
        String::from_utf8_lossy(value.into()).to_string()
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => (),
            Value::Boolean(b) => b.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => // TODO try to convert to integer
                unsafe {
                    mem::transmute::<f64, i64>(*f).hash(state)
                }
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::MidStr(s) => s.1[..s.0 as usize].hash(state),
            Value::LongStr(s) => s.hash(state),
            Value::Table(t) => Rc::as_ptr(t).hash(state),
            Value::LuaFunction(f) => Rc::as_ptr(f).hash(state),
            Value::RustFunction(f) => (*f as *const usize).hash(state),
            Value::LuaClosure(c) => Rc::as_ptr(c).hash(state),
        }
    }
}
