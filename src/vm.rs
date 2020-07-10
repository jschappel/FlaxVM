use crate::ast::{Instruction, Value, Opcode};



pub struct VM<'vm> {
    pc: usize,
    instructions: Vec<Instruction<'vm>>,
    stack: Vec<Value<'vm>>
}

impl<'vm> VM<'vm> {
    pub fn init(instructions: Vec<Instruction<'vm>>) -> Self {
        VM { 
            pc: 0,
            stack: Vec::new(),
            instructions,
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.instructions.len() {
            self.execute_instruction()
        }
    }

    fn execute_instruction(&mut self) {
        // println!("Instruction: {:?}", &self.instructions[self.pc]);
        // println!("Stack: {:?}", &self.stack);
        match &self.instructions[self.pc] {
            Instruction::A(ref opcode) => {
                match opcode {
                    Opcode::OpReturn => { 
                        let val = &self.stack[self.stack.len() -1];
                        println!("{}", val);
                        self.pc+=1;
                    }
                    Opcode::OpAdd => {
                        let val2 = self.stack.pop().expect("Stack is empty");
                        let val1 = self.stack.pop().expect("Stack is empty");
                        match (val1 , val2) {
                            (Value::Number(v1), Value::Number(v2)) => {
                                self.stack.push(Value::Number(v2 + v1));
                                self.pc+=1;
                            },
                            _ => panic!("Either val1 or val2 was not a Value::Number"),
                        }
                    },
                    Opcode::OpSub => {
                        let val2 = self.stack.pop().expect("Stack is empty");
                        let val1 = self.stack.pop().expect("Stack is empty");
                        match (val1 , val2) {
                            (Value::Number(v1), Value::Number(v2)) => {
                                self.stack.push(Value::Number(v1 - v2));
                                self.pc+=1;
                            },
                            _ => panic!("Either val1 or val2 was not a Value::Number"),
                        }
                    },
                    Opcode::OpMult => {
                        let val2 = self.stack.pop().expect("Stack is empty");
                        let val1 = self.stack.pop().expect("Stack is empty");
                        match (val1 , val2) {
                            (Value::Number(v1), Value::Number(v2)) => {
                                self.stack.push(Value::Number(v2 * v1));
                                self.pc+=1;
                            },
                            _ => panic!("Either val1 or val2 was not a Value::Number"),
                        }
                    },
                    Opcode::OpDiv => {
                        let val2 = self.stack.pop().expect("Stack is empty");
                        let val1 = self.stack.pop().expect("Stack is empty");
                        match (val1 , val2) {
                            (Value::Number(v1), Value::Number(v2)) => {
                                self.stack.push(Value::Number(v1 / v2));
                                self.pc+=1;
                            },
                            _ => panic!("Either val1 or val2 was not a Value::Number"),
                        }
                    },
                    _ => panic!("Invalid Opcode for A_Instruction."),
                }
            },
            Instruction::B(opcode, val) => {
                match opcode {
                    Opcode::OpConstant => {
                        self.stack.push(val.clone());
                        self.pc+=1;
                    }
                    _ => panic!("Invalid Opcode for B_Instruction.")
                }
            },
        } 
        
    }
}


#[cfg(test)]
mod vm_test {
    use super::*;

    #[test]
    fn instruction_set_add() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpAdd),
        ];
        let mut vm = VM::init(instructions);
        vm.run();


        assert_eq!(vm.stack[vm.stack.len() -1], Value::Number(11.0));
    }

    #[test]
    fn instruction_set_subtract() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpSub),
        ];
        let mut vm = VM::init(instructions);
        vm.run();


        assert_eq!(vm.stack[vm.stack.len() -1], Value::Number(-1.0));
    }

    #[test]
    fn instruction_set_mult() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpMult),
        ];
        let mut vm = VM::init(instructions);
        vm.run();


        assert_eq!(vm.stack[vm.stack.len() -1], Value::Number(30.0));
    }

    #[test]
    fn instruction_set_divide() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::A(Opcode::OpDiv),
        ];
        let mut vm = VM::init(instructions);
        vm.run();


        assert_eq!(vm.stack[vm.stack.len() -1], Value::Number(1.2));
    }

    #[test]
    #[ignore]
    fn full_instruction_set() {
        use crate::lexer;
        use crate::parser;

        let lexer = lexer::Lexer::init("./tests/vm/add.flaxb").unwrap();
        let values = lexer.lex_file().unwrap();
        let parser = parser::Parser::new(values);
        let instruction_set = parser.parse_tokens().unwrap();
        let mut vm = VM::init(instruction_set);
        vm.run();

        assert_eq!(vm.stack[vm.stack.len() -1], Value::Number(30.0));
    }
}