use core::fmt;
use std::{hash::Hash, default};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualifiers {
    pub cv: Option<CVQual>, // CV / Type Qualifiers
    pub storage: StorageQual, // Storage Class
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CVQual {
    Const,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StorageQual {
    Static,
    Auto,
    Register,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]  
pub enum CType {
    #[default]
    Int,
    Char,
    Void
}

impl Default for Qualifiers {
    fn default() -> Self {
        Qualifiers { cv: None, storage: StorageQual::Auto}
    }
}

impl fmt::Display for Qualifiers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tmp: String = String::new();
        
        match self.cv {
            Some(CVQual::Const) => tmp.push_str("const "),
            Some(CVQual::Volatile) => tmp.push_str("volatile "),
            None => (),
        }
        match self.storage {
            StorageQual::Static => tmp.push_str("static"),
            StorageQual::Auto => tmp.push_str(""), //auto "), // Maybe dno't print anything here.
            StorageQual::Register => tmp.push_str("register"),
        }

        write!(f, "{tmp}")
    }
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CType::Int => write!(f, "int"),
            CType::Char => write!(f, "char"),
            CType::Void => write!(f, "void"),
        }
    }
}


impl fmt::Display for TypeSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.ctype {
            Some(ctype) =>  write!(f, "{}{}", self.qualifiers, ctype),
            None => write!(f, "{}", self.qualifiers),
        }
       
    }
}
// Weird hackiness going on here:
#[derive(Debug, Clone, Default)]
pub struct TypeSpecifier {
    pub qualifiers: Qualifiers,
    pub ctype: Option<CType>,
}

impl Hash for TypeSpecifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.qualifiers.hash(state); //DONT HASH QUALIFIERS
        self.ctype.hash(state);
    }
}

impl Eq for TypeSpecifier {}

impl PartialEq for TypeSpecifier {
    fn eq(&self, other: &Self) -> bool {
        self.ctype == other.ctype
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclaratorPart {
    FunctionDecl(CType),
    PointerDecl(Option<CType>),
    ArrayDecl(usize),
}

impl DeclaratorPart {
    pub fn size(&self) -> usize {
        match self {
            DeclaratorPart::FunctionDecl(_) => 1,
            DeclaratorPart::PointerDecl(_) => 1,
            DeclaratorPart::ArrayDecl(size) => *size,
        }
    }
}

 
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Type {
    pub declarator: Vec<DeclaratorPart>,
    pub specifier: TypeSpecifier,
    
}

pub struct TypeWithQuals {
    pub qualifiers: Qualifiers,
    
}


impl Type {
    pub fn new() -> Type {
        Type {
            declarator: Vec::new(),
            specifier: TypeSpecifier::default(),
        }
    }

    pub fn calculate_size(&self) -> usize {
        // Don't do bytes vs other stuff for now
        match self.declarator.first() {
            Some(decl) => decl.size(),
            None => 1
        }
    }

    pub fn is_array(&self) -> bool {
        match self.declarator.first() {
            Some(DeclaratorPart::ArrayDecl(_)) => true,
            _ => false
        }
    }
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        
        fn print_declarator(declarator: &mut Vec<DeclaratorPart>, prev_fnc_or_array: bool) -> String {
            let mut buffer: String = String::new();
            let part = declarator.pop();

            if part.is_none() {
                return buffer;
            }
            let part = part.unwrap();

            match part {
                DeclaratorPart::FunctionDecl(_) => {todo!()}
                DeclaratorPart::ArrayDecl(size) => {
                    buffer.push_str(&print_declarator(declarator, true));
                    buffer.push_str(&format!("[{size}]"));
                    buffer
                }
                DeclaratorPart::PointerDecl(part) => {
                    if prev_fnc_or_array {
                        buffer.push('(');
                    }
                    buffer.push('*');
                    buffer.push_str(&print_declarator(declarator, false));
                    if prev_fnc_or_array {
                        buffer.push(')');
                    }
                    buffer
                }
            }
        }
        let string = print_declarator(& mut self.declarator.clone(), false);

        write!(f, "{:}{string}", self.specifier)
    }
    
}




