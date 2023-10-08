#![deny(rust_2018_idioms)]

#[macro_use]
extern crate lazy_static;

pub mod hir;
pub mod hirgen;
pub mod lower;
pub mod lir;
pub mod lirgen;

pub struct TypedArena<T, G> {
    pub data: Vec<T>,
    shut_up: G,
    size: usize,
}