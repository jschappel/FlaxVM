//! This is the documentation page for the Flax Virtual Machine used for the [Flax Programming Language](https://github.com/jschappel/Flax). The Flax VM is a stack based virtual machine
//! that I heave been building in my free time. If you would like to try it out, please visit the [Quick Start](#quick-start) section.
//! The VM's primary use is to interpret the flax bytecode. The Virtual machine consists of a few moving parts
//! - [Lexer](./lexer/index.html) — Contains all the functions for tokenizing the bytecode
//! - [Parser](./parser/index.html) — Parses the tokens into instructions for the virtual machine to read
//! - [VM](./vm/index.html) executes — the instruction set and returns the output.
//!
//!
//!
//! # Flax Bytecode Structure
//! There currently are two instruction variants of flax bytecode:
//! 1) **[A_Instruction](#bytecode-a_instructions)** — A single part instruction (i.e. add, sub, mult, div...).
//! 2) **[b_Instruction](#bytecode-b_instructions)** — A two part instruction (i.e. push 10).
//!
//! The current Lexer and Parser also support single line comments within the byte code. They are denoted by '#'
//!
//! # ByteCode Example:
//!
//! (10 + 2) * 20
//! ```
//! push 10
//! push 2
//! add
//! push 20
//! mult
//! 
//! ret
//! ```
//!
//! # Bytecode A_Instructions
//!
//! Below are all the valid A_Instructions:
//! - **`add`** —  pops 2 elements off the stack and adds them, then pushes the result onto the stack.
//! - **`sub`** — pops 2 elements off the stack and subtracts them, then pushes the result onto the stack.
//! - **`mult`** — pops 2 elements off the stack and multiplies them, then pushes the result onto the stack.
//! - **`div`** — pops 2 elements off the stack and divides them, then pushes the result onto the stack.
//! - **`ret`** — returns the first element that is on the stack.
//!
//!
//!
//! # ByteCode B_Instructions
//! Below are all the valid A_Instructions:
//! - **`push <val>`** pushes the `val` onto the stack.
//!
//!
//! # Quick Start
//! In order to run some bytecode in the virtual machine
//!
//TODO: FINISH THIS SECTION
mod ast;
mod lexer;
mod parser;
mod vm;
mod interpreter;


use vm::VM;

fn main() {
    let lexer = lexer::Lexer::init("./tests/vm/add.flaxb").unwrap();
    let values = lexer.lex_file().unwrap();
    let parser = parser::Parser::new(values);
    let instruction_set = parser.parse_tokens().unwrap();
    let mut vm = VM::init(instruction_set);
    vm.run();

    //println!("{:?}", parse_values);
}
