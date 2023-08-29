#![deny(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

pub mod parser;
pub mod ast;
pub mod lexer;
pub mod analysis;
pub mod error;

mod token;
mod types;
mod strings;
