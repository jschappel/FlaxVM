use std::fmt;
use std::iter::Peekable;

/// Custom Results type for the Lexer
type Result<T> = std::result::Result<T, LexError>;

/// Abstract type for a vector of tokens. Used for clarity.
type Tokens<'generic> = Vec<Token<'generic>>;

/// Represents all the types of errors can occur during lexical analysis.
///
/// # Variants
///
/// * InvalidNumber(String, usize) - A number that contains invalid characters (i.e. 3j).
/// * InvalidLiteral(String, usize) - A literal that contains invalid characters (i.e. add?).
/// * InvalidLine(usize) - Occurs if the text contains non ascii characters.
/// * InvalidFile(String) - Occurs if the path the given file is not valid.
#[derive(Debug, PartialEq)]
pub enum LexError {
    InvalidNumber(String, usize),
    InvalidLiteral(String, usize),
    InvalidLine(usize),
    InvalidFile(String),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::InvalidNumber(msg, line) => {
                write!(f, "Invalid token(number) at line {}: {}", line, msg)
            }
            LexError::InvalidLiteral(msg, line) => {
                write!(f, "Invalid token(literal) at line {}: {}", line, msg)
            }
            LexError::InvalidLine(line) => write!(f, "Unable to read line at line: {}", line),
            LexError::InvalidFile(msg) => write!(f, "Could not find file {}", msg),
        }
    }
}

/// A representation of the different tokens that the lexer can create.
#[derive(Debug, PartialEq)]
pub enum TokenType {
    Literal,
    Number,
}
/// A representation of a Token that the lexer creates from raw text.
///
///  # Fields
///
/// * 'lit' - a &str that is a pointer to the raw text that the lexer is lexing.
/// * 'token_type' - a [TokenType](enum.TokenType.html) which is the token variation that the text fits under.
/// * 'line' - a usize that represents the line that the token was found on (This is used of debugging and error handling).
#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub lit: &'a str,
    pub token_type: TokenType,
    pub line: usize,
}

impl<'a> Token<'a> {
    /// Creates a token.
    ///
    /// # Arguments
    ///
    /// * lit - a &str that is a pointer to the raw text that the lexer is lexing.
    /// * token_type - a TokenType which is the token variation that the text fits under.
    /// * line - a usize that represents the line that the token was found on.
    pub fn new_token(lit: &'a str, token_type: TokenType, line: usize) -> Self {
        Token {
            lit,
            token_type,
            line,
        }
    }
}

/// Lexer is a structure that converts raw text into tokens for the parser.
/// Lexical analysis is the first step in compiling flax byte code into the virtual machine.
/// The lexer takes the row text and then converts it into a vector of tokens. These tokens
/// are then used by the parser to build the abstract syntax tree.
///
/// The grammar for the lexer is regular and is shown below:
/// - Number: All numbers are floating point decimals
/// - Literal: A Liter is a word that consists of the characters 'a'-'z' | 'A'-'Z' | '0'-'9'.
/// A Literal MUST start with  a'-'z' | 'A'-'Z'
///
pub struct Lexer {
    // The raw text that is read from a file
    raw: String,
}

impl Lexer {
    /// Creates a Lexer structure with the give file path.
    /// If the path does not exist then a LexError::InvalidFile is returned.
    ///
    ///  # Arguments
    ///
    /// * 'path' - A string slice that holds the path to the file to be tokenized
    ///
    /// # Example
    /// ```
    /// use lexer;
    /// lexer::init("path/to/file");
    /// ```
    pub fn init(path: &str) -> Result<Self> {
        let text = std::fs::read_to_string(path)
            .or_else(|_| Err(LexError::InvalidFile(format!("File not found at {}", path))))?;
        Ok(Lexer { raw: text })
    }

    /// Tokenizes the raw text that is stored in the lexer.
    ///
    /// # Example
    /// ```
    /// use lexer;
    /// let lexer = lexer::init("path/to/file");
    /// let tokens = lexer.lex_file();
    /// ```
    ///
    pub fn lex_file(&self) -> Result<Tokens> {
        let mut tokens = Vec::new();
        for (i, line) in self.raw.lines().enumerate() {
            tokens.append(&mut lex_line(line, i + 1)?);
        }
        Ok(tokens)
    }
}

// Lexes a single line. Used by lex_file and for testing
fn lex_line(line: &str, line_number: usize) -> Result<Tokens> {
    let mut chars = line.char_indices().peekable();
    let mut tokens: Tokens = Vec::new();
    while let Some((i, c)) = chars.peek() {
        match c {
            '0'..='9' | '.' => lex_number(*i, &mut chars, line, &mut tokens, line_number)?,
            'a'..='z' | 'A'..='Z' => lex_literal(*i, &mut chars, line, &mut tokens, line_number)?,
            '#' => consume_till_end(&mut chars),
            _ => {
                chars.next();
            }
        }
    }
    Ok(tokens)
}

// Lexes a number into a token if the number is valid
fn lex_number<'long: 'short, 'short, I>(
    start: usize,
    it: &mut Peekable<I>,
    line: &'long str,
    t: &mut Tokens<'short>,
    line_num: usize,
) -> Result<()>
where
    I: Iterator<Item = (usize, char)>,
{
    let mut found_decimal = false;
    let mut cur_index = start;
    while let Some((i, c)) = it.peek() {
        match c {
            '0'..='9' => {
                cur_index = *i;
                it.next();
            }
            '.' => {
                if found_decimal {
                    return Err(LexError::InvalidNumber(
                        "Multiple decimals found in number".to_owned(),
                        line_num,
                    ));
                } else {
                    found_decimal = true;
                    cur_index = *i;
                    it.next();
                }
            }
            ' ' | '#' => break,
            _ => {
                return Err(LexError::InvalidNumber(
                    format!("Invalid character '{}' for number", c),
                    line_num,
                ))
            }
        }
    }
    t.push(Token::new_token(
        &line[start..cur_index + 1],
        TokenType::Number,
        line_num,
    ));
    Ok(())
}

// Lexes a number into a token if the literal is valid
fn lex_literal<'a: 'b, 'b, I>(
    start: usize,
    it: &mut Peekable<I>,
    line: &'a str,
    t: &mut Tokens<'b>,
    line_num: usize,
) -> Result<()>
where
    I: Iterator<Item = (usize, char)>,
{
    let mut cur_index = start;
    while let Some((i, c)) = it.peek() {
        match c {
            'a'..='z' | 'A'..='z' | '0'..='9' => {
                cur_index = *i;
                it.next();
            }
            ' ' | '#' => break,
            _ => {
                return Err(LexError::InvalidLiteral(
                    format!("Invalid character '{}' for literal", c),
                    line_num,
                ))
            }
        }
    }
    t.push(Token::new_token(
        &line[start..cur_index + 1],
        TokenType::Literal,
        line_num,
    ));
    Ok(())
}

// Eating tokens until a newline character is found
fn consume_till_end<I: Iterator<Item = (usize, char)>>(it: &mut Peekable<I>) {
    while let Some(_) = it.next() { /* Eat the comment */ }
}

#[cfg(test)]
mod test_lex_line {
    use super::*;

    //* Numbers
    #[test]
    fn number_basic() {
        let tokens = lex_line("123", 1);
        let expected = vec![Token::new_token("123", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn number_single_digit_() {
        let tokens = lex_line("1", 1);
        let expected = vec![Token::new_token("1", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn number_all_valid_characters() {
        let tokens = lex_line("0123456789", 1);
        let expected = vec![Token::new_token("0123456789", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    //* Decimals
    #[test]
    fn decimal_basic() {
        let tokens = lex_line("1.23", 1);
        let expected = vec![Token::new_token("1.23", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn decimal_with_leading_dot() {
        let tokens = lex_line(".23", 1);
        let expected = vec![Token::new_token(".23", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn decimal_all_valid_characters() {
        let tokens = lex_line("0123456789.123456789", 1);
        let expected = vec![Token::new_token(
            "0123456789.123456789",
            TokenType::Number,
            1,
        )];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn decimal_invalid() {
        let error = lex_line("0.1.2", 1);
        let expected = Err(LexError::InvalidNumber(
            "Multiple decimals found in number".to_owned(),
            1,
        ));
        assert_eq!(expected, error);
    }

    #[test]
    fn number_invalid_has_letter() {
        let error = lex_line("0.1A", 1);
        let expected = Err(LexError::InvalidNumber(
            "Invalid character 'A' for number".to_owned(),
            1,
        ));
        assert_eq!(expected, error);
    }

    #[test]
    fn decimal_with_comment_no_space() {
        let tokens = lex_line("1.2#comment starts here!", 1);
        let expected = vec![Token::new_token("1.2", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn decimal_with_comment_with_space() {
        let tokens = lex_line("1.2 #comment starts here!", 1);
        let expected = vec![Token::new_token("1.2", TokenType::Number, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    //* Literals
    #[test]
    fn literal_basic() {
        let tokens = lex_line("add", 1);
        let expected = vec![Token::new_token("add", TokenType::Literal, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn literal_with_numbers() {
        let tokens = lex_line("add1", 1);
        let expected = vec![Token::new_token("add1", TokenType::Literal, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn literal_with_comment_no_space() {
        let tokens = lex_line("add#This is the comment", 1);
        let expected = vec![Token::new_token("add", TokenType::Literal, 1)];
        assert_eq!(expected, tokens.unwrap());
    }

    #[test]
    fn literal_with_comment_with_space() {
        let tokens = lex_line("add #This is the comment", 1);
        let expected = vec![Token::new_token("add", TokenType::Literal, 1)];
        assert_eq!(expected, tokens.unwrap());
    }
}

#[cfg(test)]
mod test_lex_file {
    use super::*;

    macro_rules! lex_file_instruction_set {
        ($([$lit:expr, $num1:expr, $num2:expr, $line:expr]),*) => {
          {
              let mut ans = Vec::new();
              $(
                  ans.push(Token::new_token($lit, TokenType::Literal, $line));
                  ans.push(Token::new_token($num1, TokenType::Number, $line));
                  ans.push(Token::new_token($num2, TokenType::Number, $line));
              )*
              ans
          }
        };
    }

    #[test]
    fn lex_file_math() {
        let lexer = Lexer::init("./tests/lexer/math.flaxb").unwrap();
        let actual = lexer.lex_file().unwrap();
        let expected = lex_file_instruction_set!(
            ["add", "1", "4", 2],
            ["sub", "123", "567", 3],
            ["div", "3", "4", 4],
            ["mult", "4", "7", 5],
            ["add", "1.1", "4.56787", 8],
            ["sub", "123.1", "567.123", 9],
            ["div", "3.43", "4.233", 10],
            ["mult", "4.1", "7.54", 11],
            ["add", "1", "4.3", 14],
            ["sub", "0.123", "567", 15],
            ["div", ".3", "4", 16],
            ["mult", "4", "7.1", 17]
        );
        assert_eq!(expected, actual);
    }
}