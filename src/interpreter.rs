// use crate::parser::Parser;
// use crate::ast::{Instruction, Opcode};
// use std::convert::From;

// // a portion of bytecode
// // first 8 bits is the opcode, next 8 bits is the number
// pub struct ByteCode {
//     code: Vec<u8>
// }

// pub struct Chunk(Vec<u8>)

// impl From<Instruction<'_>> for Chunk {
//     fn from(instruction: Instruction<'_>) -> Vec<u8> {
//         match instruction {
//             Instruction::A(instr) => {
//                 match instr.opcode {
//                     Opcode::OpAdd => {
//                         vec![0, ]
//                     }
//                 }
//             }
//             _ => panic!("Not supported yet"),
//         }
//     }
// }

// fn interperate<'a>(instructions: Vec<Instruction<'a>>) {
//     for instruction in instructions {

//     }
// }
