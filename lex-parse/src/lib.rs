#![deny(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

pub mod parser;
pub mod ast;
pub mod types;
pub mod lexer;

// These might get moved to global util crate.
pub mod context;
pub mod error;
pub mod strings;

mod token;


