use core::fmt;

use lex_parse::{context::{InternedType, InternedString, Context}, ast::ASTNodeHandle, error::AnalysisError};
use slotmap::SparseSecondaryMap;


pub struct STScope {
    pub next_param_slot: i32,
    pub next_variable_slot: i32,
    pub var_entries: Vec<Declaration>,
    pub is_global: bool,
    // tag_entries: Vec<Declaration>, TODO
    // label_entries: TODO
}


impl STScope {
    pub fn new() -> STScope {
        STScope {
            next_param_slot: 0,
            next_variable_slot: 0,
            var_entries: Vec::new(),
            is_global: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclarationType {
    VarOrParam,
    Function,
    Tag,
    Label,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub identifier: InternedString, // TODO Make this a string
    pub size: usize,
    pub offset: i32,
    pub type_info: InternedType,
    pub is_global: bool,
    pub(crate) kind: DeclarationType
} 

pub(crate) struct DeclarationPrintable<'a> {
    pub(crate) declaration: Declaration,
    pub(crate) context: &'a Context<'a>
}

impl fmt::Display for DeclarationPrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let decl = &self.declaration;
        let type_info = self.context.resolve_type(decl.type_info);
        write!(f, "size: {}, offset: {}, type_info: {}, kind: {:?}, global: {}", decl.size, decl.offset, type_info, decl.kind, decl.is_global) //self.type_info, self.kind)
    }
}

pub struct SymbolTable {
    // Maps Declarations AND symbol refs to Declarations.
    pub entries: SparseSecondaryMap<ASTNodeHandle, Declaration>,
    pub stack: Vec<STScope>, // Reference to a STScope
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut st = SymbolTable { entries: SparseSecondaryMap::new(), stack: Vec::new()};
        let mut global_scope = STScope::new();
        global_scope.is_global = true;
        st.stack.push(global_scope);
        st
    }

    pub fn search_scope(&mut self, identifier: &InternedString, entry_type: &DeclarationType) -> Option<Declaration> {
        for entry in self.stack.last().unwrap().var_entries.as_slice() {
            if (entry.identifier == *identifier) && (*entry_type == entry.kind) {
                return Some(entry.clone())
            }
        }
        None
    } 

    pub fn search_up(&mut self, identifier: &InternedString, entry_type: &DeclarationType) -> Option<Declaration> {
        // TODO: Make entry types match.
        let entry = self.search_scope(&identifier, &entry_type);

        for scope in self.stack.iter().rev(){
            for entry in scope.var_entries.as_slice() {
                if (entry.identifier == *identifier) && (*entry_type == entry.kind) {
                    return Some(entry.clone())
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
            self.stack.last_mut().unwrap().var_entries.push(entry.clone());
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
