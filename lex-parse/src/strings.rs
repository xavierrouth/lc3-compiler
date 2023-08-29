#[allow(non_upper_case_globals)]

use std::{sync::Mutex};

// Do a custom string interner eventually.
use string_interner::{backend::StringBackend, symbol::SymbolU16, StringInterner};

pub type InternedString = SymbolU16;

lazy_static! {
    pub static ref Strings: Mutex<StringInterner<StringBackend<SymbolU16>>> = Mutex::new(StringInterner::new());
}