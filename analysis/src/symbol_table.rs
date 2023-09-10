use core::fmt;
use std::ops::Deref;

use lex_parse::{context::{InternedType, InternedString, Context}, ast::ASTNodeHandle, error::AnalysisError, types::TypePrintable};
use slotmap::{SparseSecondaryMap, SlotMap};

pub struct Scope {
    pub next_param_slot: i32,
    pub next_variable_slot: i32,
    pub declarations: Vec<Declaration>,
    pub is_global: bool,
    pub records: Vec<Record>
    // label_entries: TODO
}

impl Scope {
    pub fn new(next_param_slot: i32, next_variable_slot: i32) -> Scope {
        Scope {
            next_param_slot,
            next_variable_slot,
            declarations: Vec::new(),
            records: Vec::new(),
            is_global: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclarationType {
    Var, 
    Param,
    Function,
    Tag,
    Label,
    Field,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub identifier: InternedString, 
    pub size: usize,
    pub offset: i32,
    pub type_info: InternedType,
    pub is_global: bool,
    pub kind: DeclarationType
} 

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub identifier: InternedString,
    pub size: usize,
    pub fields: Vec<Declaration>, // Declaration, Offset 
}

pub struct SymbolTable {
    // Maps Declarations AND symbol refs to Declarations.
    pub records: Vec<Record>, // This is basically a type. // Global Records only
    pub entries: SparseSecondaryMap<ASTNodeHandle, Declaration>,
    pub stack: Vec<Scope>, // Reference to a Scope
}


impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut st = SymbolTable { entries: SparseSecondaryMap::new(), stack: Vec::new(), records: Vec::new()};
        let mut global_scope = Scope::new(0, 0);
        global_scope.is_global = true;
        st.stack.push(global_scope);
        st
    }

    pub fn search_scope(&mut self, identifier: &InternedString, entry_type: &DeclarationType) -> Option<Declaration> {
        for entry in self.stack.last().unwrap().declarations.as_slice() {
            if (entry.identifier == *identifier) && (*entry_type == entry.kind) {
                return Some(entry.clone())
            }
        }
        None
    } 

    // TODO: search for structs vs enums vs unions.
    pub fn search_record(&mut self, identifier: &InternedString) -> Option<Record> {
        for record in &self.records {
            if *identifier == record.identifier {
                return Some(record.clone());
            }
        }
        None
    }

    pub fn search_up(&mut self, identifier: &InternedString, entry_type: &DeclarationType) -> Option<Declaration> {
        let entry = self.search_scope(&identifier, &entry_type);

        for scope in self.stack.iter().rev(){
            for entry in scope.declarations.as_slice() {
                if *entry_type == DeclarationType::Var || *entry_type == DeclarationType::Param {
                    if (entry.identifier == *identifier) && (entry.kind == DeclarationType::Var || entry.kind == DeclarationType::Param) {
                        return Some(entry.clone())
                    }
                }
                else {
                    if entry.identifier == *identifier && entry.kind == *entry_type {
                        return Some(entry.clone())
                    }
                }
            }
        }
        entry
    }

    pub fn add(&mut self, node: ASTNodeHandle, entry: Declaration) -> Result<(), AnalysisError> {
        if self.search_scope(&entry.identifier, &entry.kind).is_some() {
            Err(AnalysisError::AlreadyDeclared(entry.identifier, node))
        }
        else {
            self.stack.last_mut().unwrap().declarations.push(entry.clone());
            self.entries.insert(node, entry);
            Ok(())
        }
    }

    fn get_type(& self, node: & ASTNodeHandle) -> Option<InternedType> {
        if let Some(entry) = self.entries.get(*node) {
            Some(entry.type_info)
        }
        else {
            None
        }
    }

}


// Display Things


pub(crate) struct DeclarationPrintable<'a> {
    pub(crate) declaration: Declaration,
    pub(crate) context: &'a Context<'a>
}

impl fmt::Display for DeclarationPrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let decl = &self.declaration;
        let type_info = self.context.resolve_type(decl.type_info);
        write!(f, "size: {}, offset: {}, type_info: {}, kind: {:?}, global: {}", decl.size, decl.offset, 
        TypePrintable{data: type_info, context: self.context}, decl.kind, decl.is_global) //self.type_info, self.kind)
    }
}