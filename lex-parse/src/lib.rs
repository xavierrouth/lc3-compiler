#![deny(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

pub mod token;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod types;
pub mod strings;
pub mod analysis;