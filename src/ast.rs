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

// <Opcode> <Value> <Value>
// #[derive(Debug, PartialEq)]
// pub struct AInstruction<'a> {
//     pub opcode: Opcode,
//     pub val1: Value<'a>,
//     pub val2: Value<'a>,
//     pub line: usize,
// }

// impl<'a> AInstruction<'a> {
//     pub fn new(opcode: Opcode, val1: Value<'a>, val2: Value<'a>, line: usize) -> Self {
//         Self { opcode, val1, val2, line }
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Number(f32),
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