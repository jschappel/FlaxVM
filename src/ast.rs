#[derive(Debug, PartialEq)]
pub enum Opcode {
    OpTrue,
    OpFalse,
    OpNil,
    OpNot,
    OpReturn,
    OpNegate,
    OpGreater,
    OpLess,
    OpEqual,
    OpAdd,
    OpSub,
    OpDiv,
    OpMult,
    OpConstant,
    OpObj, // heap structures
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
    Obj(&'a str),
}

use std::fmt;
impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            _ => write!(f, "Value type does not implement display yet"),
        }
    }
}
