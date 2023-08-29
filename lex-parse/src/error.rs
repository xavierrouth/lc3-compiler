use core::fmt;
use std::error;

use crate::{token::{Token}, strings::InternedString};

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

pub fn print_parser_error(error: ParserError, line: InternedString) -> () {

}