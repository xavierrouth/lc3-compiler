use std::{collections::HashMap, fs::File, cell::RefCell, ops::Deref};

use slotmap::SecondaryMap;
// Do a custom string interner eventually.
use string_interner::{backend::StringBackend, symbol::SymbolU16, StringInterner};

use crate::{types::{Type}, error::{ErrorHandler, AnalysisError}, ast::ASTNodeHandle, token::Token};

pub type InternedString = SymbolU16;

#[derive(Debug)]
pub struct Context<'ctx> {
    strings: RefCell<StringInterner<StringBackend<InternedString>>>, 
    types: RefCell<TypeInterner>,
    
    // This is debug info:
    tokens: RefCell<SecondaryMap<ASTNodeHandle, Token>>,
    src: &'ctx str,
}

impl <'ctx> Context <'ctx>{
    pub fn new(src: &'ctx str ) -> Context<'ctx> {
        Context { src, strings: StringInterner::new().into(), tokens: SecondaryMap::new().into(), types: TypeInterner::new().into()}
    }

    /* Fixme this should just be an &str */
    pub fn resolve_string(& self, string: InternedString) -> &'ctx str  {
        self.strings.borrow().resolve(string).unwrap()
    }

    pub fn get_string(&self, string: &str) -> InternedString {
        self.strings.borrow_mut().get_or_intern(string)
    }

    pub fn resolve_type(&self, r#type: &InternedType) -> Type {
        self.types.borrow().resolve(r#type)
    }

    pub fn get_type(&self, r#type: &Type) -> InternedType {
        self.types.borrow_mut().get(r#type)
    }

    pub fn get_line(&self, line: usize) -> &str {
        self.src.lines().nth(line).unwrap()
    }

    pub fn map_token(&self, node: ASTNodeHandle, token: Token) -> () {
        self.tokens.borrow_mut().insert(node, token);
    }

    pub fn get_token(&self, node: ASTNodeHandle) -> Option<Token> {
        self.tokens.borrow().get(node).cloned()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
pub struct InternedType {
    value: u16,
}

impl Deref for InternedType {
    fn deref(&self) -> &InternedType {
        self
    }

    type Target = InternedType;
}

#[derive(Debug)]
pub struct TypeInterner {
    map: HashMap<Type, InternedType>,
    buff: Vec<Type>,
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner { map: HashMap::new(), buff: Vec::new() }
    }

    pub fn get(&mut self, r#type: &Type) -> InternedType {
        if let Some(handle) = self.map.get(r#type) {
            *handle
        }
        else {
            let handle: InternedType = InternedType {value: self.map.len() as u16 }; /* This is unsafe no? */
            self.map.insert(r#type.to_owned(), handle);
            self.buff.push(r#type.to_owned());

            debug_assert!(self.buff[handle.value as usize] == *r#type);
            handle
        }
    }

    pub fn resolve(&self, idx: &InternedType) -> Type {
        self.buff[idx.value as usize].to_owned()
    }
   
}