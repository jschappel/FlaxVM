#[derive(Debug, PartialEq)]
pub enum Opcode {
    OpAdd,
    OpSub,
    OpDiv,
    OpMult,
    OpConstant,
    OpReturn,
}

#[derive(Debug, PartialEq)]
pub enum Instruction<'a> {
    A(Opcode),            // Single instruction: add, mult, sub, div
    B(Opcode, Value<'a>), // Two part instruction: push 1
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Number(f32),
    Boolean(bool),
    Nil,
    Str(&'a str),
}

use std::fmt;
impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            _ => write!(f, "Value type does not implement display yet"),
        }
    }
}
