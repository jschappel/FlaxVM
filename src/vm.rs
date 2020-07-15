use crate::ast::{Instruction, Opcode, Value};

pub struct VM<'vm> {
    pc: usize,
    instructions: Vec<Instruction<'vm>>,
    stack: Vec<Value<'vm>>, // Note: The stack aso can contain ptrs to the heap
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
            Instruction::A(ref opcode) => match opcode {
                Opcode::OpReturn => {
                    let val = &self.stack[self.stack.len() - 1];
                    println!("{}", val);
                    self.pc += 1;
                },
                Opcode::OpTrue => {
                    self.stack.push(Value::Boolean(true));
                    self.pc+=1;
                }
                Opcode::OpFalse => {
                    self.stack.push(Value::Boolean(true));
                    self.pc+=1;
                },
                Opcode::OpNil => {
                    self.stack.push(Value::Nil);
                    self.pc+=1;
                },
                Opcode::OpEqual => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    let ans = Self::check_equality(val1, val2);
                    self.stack.push(Value::Boolean(ans));
                    self.pc+=1;
                },
                Opcode::OpGreater => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Boolean(v1 > v2));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                Opcode::OpLess => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Boolean(v1 < v2));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                Opcode::OpNot => {
                    if let Some(Value::Boolean(_)) = self.peek() {
                        match self.stack.pop().unwrap() {
                            Value::Boolean(val) => {
                                self.stack.push(Value::Boolean(!val));
                                self.pc+=1;
                            },
                            _ => unreachable!(),
                        }
                    }
                },
                Opcode::OpNegate => {
                    if let Some(Value::Number(_)) = self.peek() {
                        match self.stack.pop().unwrap() {
                            Value::Number(x) => {  
                                self.stack.push(Value::Number(x * -1.0));
                                self.pc+=1;
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        panic!("Stack is empty or value being negated is not a number");
                    }
                },
                Opcode::OpAdd => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Number(v2 + v1));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                Opcode::OpSub => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Number(v1 - v2));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                Opcode::OpMult => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Number(v2 * v1));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                Opcode::OpDiv => {
                    let val2 = self.stack.pop().expect("Stack is empty");
                    let val1 = self.stack.pop().expect("Stack is empty");
                    match (val1, val2) {
                        (Value::Number(v1), Value::Number(v2)) => {
                            self.stack.push(Value::Number(v1 / v2));
                            self.pc += 1;
                        }
                        _ => panic!("Either val1 or val2 was not a Value::Number"),
                    }
                },
                _ => panic!("Invalid Opcode for A_Instruction."),
            },
            Instruction::B(opcode, val) => match opcode {
                Opcode::OpConstant => {
                    self.stack.push(*val);
                    self.pc += 1;
                }
                _ => panic!("Invalid Opcode for B_Instruction."),
            },
        }
    }

    // Peeks at the top item on the stack
    fn peek(&self) -> Option<&Value<'vm>> {
        if self.stack.len() > 0 {
            return Some(&self.stack[0])
        }
        None
    }

    // Determines if two values are equal to each other
    fn check_equality(val1: Value, val2: Value) -> bool {
        match (val1, val2) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false
        }
    }

    // This function is used for testing only. Not production
    #[allow(dead_code)]
    fn reset(&mut self, instructions: Vec<Instruction<'vm>>) {
        self.pc = 0;
        self.stack = Vec::new();
        self.instructions = instructions;
    }

    // ONLY USE FOR TESTING PURPOSES
    #[allow(dead_code)]
    fn test_extract_value(&self) -> Option<Value<'vm>> {
        if let Some(v) = self.peek() {
            return Some(*v)
        }
        None
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

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(11.0));
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

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(-1.0));
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

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(30.0));
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

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(1.2));
    }

    #[test]
    fn negate_number() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::A(Opcode::OpNegate),
        ];
        let mut vm = VM::init(instructions);
        vm.run();

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(-5.0));
    }

    #[test]
    fn not_boolean() {
        let instructions = vec![
            Instruction::B(Opcode::OpConstant, Value::Boolean(true)),
            Instruction::A(Opcode::OpNot),
        ];
        let mut vm = VM::init(instructions);
        vm.run();

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Boolean(false));
    }

    #[test]
    fn equals_number() {
        let instructions_eq = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(2.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(2.0)),
            Instruction::A(Opcode::OpEqual),
        ];
        let instructions_greater = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(4.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(2.0)),
            Instruction::A(Opcode::OpGreater),
        ];
        let instructions_less = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(2.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(100.0)),
            Instruction::A(Opcode::OpLess),
        ];
        let mut vm = VM::init(instructions_eq);
        vm.run();
        let assert_equal = vm.test_extract_value();
        vm.reset(instructions_greater);
        vm.run();
        let assert_greater = vm.test_extract_value();
        vm.reset(instructions_less);
        vm.run();
        let assert_less = vm.test_extract_value();

        assert_eq!(assert_equal.unwrap(), Value::Boolean(true));
        assert_eq!(assert_greater.unwrap(), Value::Boolean(true));
        assert_eq!(assert_less.unwrap(), Value::Boolean(true));
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

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(30.0));
    }

    #[test]
    #[ignore]
    fn negative_instruction_set() {
        use crate::lexer;
        use crate::parser;

        let lexer = lexer::Lexer::init("./tests/vm/negatives.flaxb").unwrap();
        let values = lexer.lex_file().unwrap();
        let parser = parser::Parser::new(values);
        let instruction_set = parser.parse_tokens().unwrap();
        let mut vm = VM::init(instruction_set);
        vm.run();

        assert_eq!(vm.stack[vm.stack.len() - 1], Value::Number(-20.0));
    }
}
