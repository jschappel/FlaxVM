use crate::ast::{Instruction, Opcode, Value};
use crate::lexer::{Token, TokenType};
use std::fmt;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    FloatParseError(String, usize),
    InvalidNumber(String, usize),
    InvalidOpcode(String, usize),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidNumber(msg, line) => write!(f, "Error line {}: {}", msg, line),
            ParseError::InvalidOpcode(msg, line) => write!(f, "Error line {}: {}", msg, line),
            ParseError::FloatParseError(msg, line) => write!(f, "Error line {}: {}", msg, line),
        }
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser { tokens }
    }

    pub fn parse_tokens(&'a self) -> Result<Vec<Instruction>, ParseError> {
        let mut it = self.tokens.iter().peekable();
        let mut instructions: Vec<Instruction<'a>> = Vec::new();
        while let Some(_) = it.peek() {
            let instruction = Self::build_instruction(&mut it)?;
            instructions.push(instruction);
        }
        Ok(instructions)
    }

    fn build_instruction<I>(it: &mut Peekable<I>) -> Result<Instruction<'a>, ParseError>
    where
        I: Iterator<Item = &'a Token<'a>>,
    {
        let token: &Token = it.next().unwrap();
        match token.token_type {
            TokenType::Literal => {
                return match token.lit {
                    "add" => Ok(Self::build_a_instruction(Opcode::OpAdd)),
                    "sub" => Ok(Self::build_a_instruction(Opcode::OpSub)),
                    "mult" => Ok(Self::build_a_instruction(Opcode::OpMult)),
                    "div" => Ok(Self::build_a_instruction(Opcode::OpDiv)),
                    "ret" => Ok(Self::build_a_instruction(Opcode::OpReturn)),
                    "push" => Ok(Self::build_b_instruction(Opcode::OpConstant, it)?),
                    _ => Err(ParseError::InvalidOpcode(
                        format!("Invalid OpCode. Given: '{}'", token.lit),
                        token.line,
                    )),
                };
            },
            _ => Err(ParseError::InvalidOpcode(
                format!("Invalid Token type. Given: '{}'", token.lit),
                token.line,
            )),
        }
    }

    fn build_a_instruction(opcode: Opcode) -> Instruction<'a> {
        Instruction::A(opcode)
    }

    fn build_b_instruction<I>(
        opcode: Opcode,
        it: &mut Peekable<I>,
    ) -> Result<Instruction<'a>, ParseError>
    where
        I: Iterator<Item = &'a Token<'a>>,
    {
        if let Some(token) = it.peek() {
            return match token.token_type {
                TokenType::Nil => {
                    it.next();
                    Ok(Instruction::B(opcode, Value::Nil))
                },
                TokenType::True => {
                    it.next();
                    Ok(Instruction::B(opcode, Value::Boolean(true)))
                },
                TokenType::False => {
                    it.next();
                    Ok(Instruction::B(opcode, Value::Boolean(false)))
                },
                TokenType::Number => {
                    Ok(Self::build_number_instr(opcode, it)?)
                }
                _ => Err(ParseError::InvalidNumber(format!("Expected number given '{}'", token.lit), token.line,)),
            }
        }
        Err(ParseError::InvalidNumber("Expected token given nothing".to_owned(), 1))
    }

    fn build_number_instr<I>(opcode: Opcode, it: &mut Peekable<I>) -> Result<Instruction<'a>, ParseError>
    where
        I: Iterator<Item = &'a Token<'a>>
    {
        // It is safe to unwarp here because build_b_instruction checks to make sure
        //  that the iterator is not empty.
        match it.peek().unwrap().token_type {
            TokenType::Number => {
                let token1 = it.next().unwrap();
                let parsed_val = token1.lit.parse::<f32>().or_else(|_| {
                    Err(ParseError::FloatParseError(
                        format!("Unable to parse '{}' to a float", token1.lit),
                        token1.line,
                    ))
                })?;
                let val1 = Value::Number(parsed_val);
                return Ok(Instruction::B(opcode, val1));
            },
            _ => {
                let val = it.next().unwrap();
                Err(ParseError::InvalidNumber(format!("Unable to parse '{}' to a number.", val.lit), val.line))
            },
        }
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn negative_number() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("-5", TokenType::Number, 1),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![Instruction::B(Opcode::OpConstant, Value::Number(-5.0))];
        assert_eq!(expected, actual);
    }


    #[test]
    fn a_instruction_add() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("5", TokenType::Number, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("6", TokenType::Number, 2),
            Token::new_token("add", TokenType::Literal, 3),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpAdd),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn a_instruction_sub() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("5", TokenType::Number, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("6", TokenType::Number, 2),
            Token::new_token("sub", TokenType::Literal, 3),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpSub),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn a_instruction_div() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("5", TokenType::Number, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("6", TokenType::Number, 2),
            Token::new_token("div", TokenType::Literal, 3),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpDiv),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn a_instruction_mult() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("5", TokenType::Number, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("6", TokenType::Number, 2),
            Token::new_token("mult", TokenType::Literal, 3),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(6.0)),
            Instruction::A(Opcode::OpMult),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn boolean_opcode() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("true", TokenType::False, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("false", TokenType::True, 2),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Boolean(false)),
            Instruction::B(Opcode::OpConstant, Value::Boolean(true)),
        ];
        assert_eq!(expected, actual);

    }

    #[test]
    fn nil_opcode() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("nil", TokenType::Nil, 1),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens().unwrap();
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Nil),
        ];
        assert_eq!(expected, actual);

    }

    #[test]
    fn invalid_opcode() {
        let tokens_for_instr = vec![
            Token::new_token("hello", TokenType::Literal, 1),
            Token::new_token("5", TokenType::Number, 1),
            Token::new_token("push", TokenType::Literal, 2),
            Token::new_token("6", TokenType::Number, 2),
            Token::new_token("mult", TokenType::Literal, 3),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens();
        let expected = Err(ParseError::InvalidOpcode(
            format!("Invalid OpCode. Given: '{}'", "hello"),
            1,
        ));
        assert_eq!(expected, actual);
    }

    //TODO: only an error till I add strings
    #[test]
    fn invalid_constant() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("Josh", TokenType::Literal, 1),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens();
        let expected = Err(ParseError::InvalidNumber(
            format!("Expected number given '{}'", "Josh"),
            1,
        ));
        assert_eq!(expected, actual);
    }

    #[test]
    fn invalid_float_parse() {
        let tokens_for_instr = vec![
            Token::new_token("push", TokenType::Literal, 1),
            Token::new_token("Josh", TokenType::Number, 1),
        ];
        let parser = Parser::new(tokens_for_instr);
        let actual = parser.parse_tokens();
        let expected = Err(ParseError::FloatParseError(
            format!("Unable to parse '{}' to a float", "Josh"),
            1,
        ));
        assert_eq!(expected, actual);
    }

    #[test]
    fn instruction_set_parser_file() {
        use crate::lexer;
        let tokens = lexer::Lexer::init("./tests/parser/add.flaxb").unwrap();
        let tokens = tokens.lex_file().unwrap();
        let actual = Parser::new(tokens);
        println!("{:?}", actual.tokens);
        let expected = vec![
            Instruction::B(Opcode::OpConstant, Value::Number(5.0)),
            Instruction::B(Opcode::OpConstant, Value::Number(10.0)),
            Instruction::A(Opcode::OpAdd),
            Instruction::B(Opcode::OpConstant, Value::Number(3.0)),
            Instruction::A(Opcode::OpAdd),
        ];
        assert_eq!(expected, actual.parse_tokens().unwrap());
    }
}
