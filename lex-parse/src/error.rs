use core::fmt;
use std::error;

use colored::Colorize;
use slotmap::SecondaryMap;

use crate::{token::{Token}, strings::{InternedString, Strings}, ast::ASTNodeHandle};

// This is debug info at this point.


#[derive(Debug)]
pub enum ParserError {
    FloatError(String),
    GeneralError(String, Option<Token>),
    MissingToken(String, Option<Token>),
    UnknownError
}

impl<'a> error::Error for ParserError {}

impl<'a> fmt::Display for ParserError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!() // Don't call this.
    }
}

#[derive(Debug)]
pub enum LexerError {
    FloatError(String),
    UnknownError
}

impl error::Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::FloatError(msg) => write!(f, "{}", msg),
            LexerError::UnknownError => write!(f, "Something went wrong"),
        }
    }
}

pub struct ErrorHandler {
    pub tokens: SecondaryMap<ASTNodeHandle, Token>,  
    pub lines: Vec<InternedString>,
}

impl ErrorHandler {
    pub fn new() -> ErrorHandler {
        ErrorHandler {
            tokens: SecondaryMap::new(),
            lines: Vec::new(),
        }
    }

    pub fn print_parser_error(&self, error: ParserError) -> () {
        
        fn get_whitespace(count: usize) -> String {
            std::iter::repeat(' ').take(count).collect()
        }

        match error {
            ParserError::FloatError(msg) => println!( "{msg}"),
            ParserError::GeneralError(msg, dbg_info) => {
                if let Some(token) = dbg_info {
                    println!("{} {msg}", "error:".red());
                    let line = self.lines.get(token.row).unwrap();
                    let lock = Strings.lock().unwrap();
                    let line = lock.resolve(*line).unwrap();
                    print!("line {} | {} ", token.row + 1, line);
                    println!("{}{}", get_whitespace(8 + token.col), "^".green());
                }
                else {
                    println!("{msg}");
                }
            },
            ParserError::MissingToken(msg, dbg_info) => {
                if let Some(token) = dbg_info {
                    println!("{} {msg}", "error:".red());
                    let line = self.lines.get(token.row).unwrap();
                    let lock = Strings.lock().unwrap();
                    let line = lock.resolve(*line).unwrap();
                    print!("line {} | {} ", token.row + 1, line);
                    println!("{}{}", get_whitespace(7 + token.col + token.length), "^".green());
                }
                else {
                    println!("{msg}");
                }
            },
            ParserError::UnknownError => println!("Something went wrong"),
        }
            
        
    }
}