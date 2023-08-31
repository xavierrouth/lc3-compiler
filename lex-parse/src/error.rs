use core::fmt;
use std::{error, iter::successors};

use colored::Colorize;
use slotmap::SecondaryMap;

use crate::{token::{Token}, strings::{InternedString, Strings}, ast::ASTNodeHandle, analysis::AnalysisError};

// This is debug info at this point.


#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    GeneralError(String, Option<Token>),
    ExpectedConstantInt(Token),
    MissingSemicolon(Token),
    MissingDeclarator(Token),
    UnknownError
}

impl<'a> error::Error for ParserError {}

impl<'a> fmt::Display for ParserError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!() // Don't call this.
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    pub fatal: bool,
}

impl ErrorHandler {
    pub fn new() -> ErrorHandler {
        ErrorHandler {
            tokens: SecondaryMap::new(),
            lines: Vec::new(),
            fatal: false,
        }
    }

    pub fn print_error(&self, ) -> () {

    }

    pub fn print_analysis_error(&mut self, error: AnalysisError) -> () {
        self.fatal = true;
        fn get_whitespace(count: usize) -> String {
            std::iter::repeat(' ').take(count).collect()
        }

        match error {
            // TODO: Report previous declaration. No, I don't want to.
            AnalysisError::AlreadyDeclared(identifier, node_h) => {
                let token = self.tokens.get(node_h).unwrap();
                let line = self.lines.get(token.row).unwrap();


                let lock = Strings.lock().unwrap();
                let line = lock.resolve(*line).unwrap();
                let identifier = lock.resolve(identifier).unwrap();

                println!("{} redeclaration of '{}'", "error:".red(), identifier);
                print!("line {} | {} ", token.row + 1, line);
                
                let mut length: usize = "line  | ".len();
                let n = token.row + 1;
                length += successors(Some(n), |&n| (n >= 10).then(|| n / 10)).count(); // Number of spaces this int takes up.

                println!("{}{}", get_whitespace(length + token.col - 2), "^".green());

            }
            AnalysisError::UnknownSymbol(identifier, node_h) => {
                let token = self.tokens.get(node_h).unwrap();
                let line = self.lines.get(token.row).unwrap();


                let lock = Strings.lock().unwrap();
                let line = lock.resolve(*line).unwrap();
                let identifier = lock.resolve(identifier).unwrap();

                println!("{} unknown identifier: '{}'", "error:".red(), identifier);
                print!("line {} | {} ", token.row + 1, line);
                
                let mut length: usize = "line  | ".len();
                let n = token.row + 1;
                length += successors(Some(n), |&n| (n >= 10).then(|| n / 10)).count(); // Number of spaces this int takes up.

                println!("{}{}", get_whitespace(length + token.col - 2), "^".green());
            },
        }
    }



    fn print_arrow(&mut self, line_num: usize, arrow_offset: i32) -> () {
        let mut length: usize = "line  | ".len();
        let n = line_num + 1;
        length += successors(Some(n), |&n| (n >= 10).then(|| n / 10)).count(); // Number of spaces this int takes up.
        let total_offset:i32 = length as i32 + arrow_offset;

        println!("{}{}", Self::get_whitespace(total_offset.try_into().unwrap()), "^".green());
    }

    fn print_line(&mut self, line_num: usize) -> () {
        let line = self.lines.get(line_num).unwrap();
        let lock = Strings.lock().unwrap();
        let line = lock.resolve(*line).unwrap();
        print!("line {} | {} ", line_num + 1, line);
    }

    fn get_whitespace(count: usize) -> String {
        std::iter::repeat(' ').take(count).collect()
    }

    pub fn print_parser_error(&mut self, error: ParserError) -> () {
        self.fatal = true;
        
        match error {
            // TODO: Make this a different error type if there is no token, instead of Option<Token>, wasted check.
            ParserError::MissingDeclarator(token) => {
                println!("{} missing declarator", "error:".red());
                self.print_line(token.row);
                self.print_arrow(token.row, (token.col as i32 - 2).try_into().unwrap())
            },
            ParserError::GeneralError(msg, dbg_info) => {
                if let Some(token) = dbg_info {
                    println!("{} {msg}", "error:".red());
                    self.print_line(token.row);
                    self.print_arrow(token.row, token.col as i32 - 2)
                }
                else {
                    println!("{msg}");
                }
            },
            ParserError::MissingSemicolon(token) => {
                println!("{} expected semicolon", "error:".red());
                self.print_line(token.row);
                self.print_arrow(token.row, ((token.col + token.length) as i32 - 1).try_into().unwrap())
            },
            ParserError::ExpectedConstantInt(token) => {
                println!("{} complex expressions here are not enabled, please use a single int", "error:".red());
                self.print_line(token.row);
                self.print_arrow(token.row, (token.col as i32 - 2).try_into().unwrap())
            },
            ParserError::UnknownError => println!("Something went wrong"),
        }
            
        
    }
}