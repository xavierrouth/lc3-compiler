use core::fmt;
use std::{ops::Deref, rc::Rc, cell::RefCell};

use lex_parse::{context::{InternedType, InternedString, Context}, ast::ASTNodeHandle, error::AnalysisError, types::TypePrintable};
use slotmap::{SparseSecondaryMap, SlotMap};

slotmap::new_key_type! { pub struct ScopeHandle; }

pub struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) declarations: Vec<LocalVarDecl>,
}

pub enum ScopeKind {
    Function(ScopeHandle),
    Block(ScopeHandle),
    File,
}

// This could do to be a tagged enum:
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub identifier: InternedString, 
    pub type_info: InternedType,
    //TODO: info about parameters for type checking.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalVarDecl {
    pub scope: ScopeHandle, 
    pub identifier: InternedString, 
    pub size: usize,
    pub type_info: InternedType,
    pub is_parameter: bool,
} 

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVarDecl {
    pub identifier: InternedString, 
    pub size: usize,
    pub type_info: InternedType,
}

// Type Context:

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub identifier: InternedString,
    pub size: usize,
    pub type_info: InternedType
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordDecl {
    pub complete: bool,
    pub identifier: InternedString,
    pub size: usize,
    pub fields: Vec<FieldDecl>, // LocalVarDecl, Offset 
}


pub struct SymbolTable {
    // Maps Declarations AND symbol refs to Declarations.
    // No codegen required:
    pub(crate) records: Vec<RecordDecl>, // This is basically a type. // Global Records only

    // Codegen required:
    pub(crate) functions: Vec<FunctionDecl>,
    pub(crate) globals: Vec<GlobalVarDecl>,

    pub root: Option<ScopeHandle>,

    pub(crate) scope_arena: SlotMap<ScopeHandle, Scope>,

    // TODO: Make  
    //pub locals: Vec<LocalVarDecl>,
}

pub enum DeclRef {
    LocalVar(LocalVarDecl),
    Field(FieldDecl),
    Function(FunctionDecl),
    Global(GlobalVarDecl),
}

impl <'a> SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {  
            records: Vec::new(), 
            functions: Vec::new(), 
            globals: Vec::new(),
            root: None,
            scope_arena: SlotMap::with_key(),
        }
    }

    pub fn resolve_scope(&self, scope_h: ScopeHandle) -> &Scope {
        self.scope_arena.get(scope_h).expect("invalid handle")
    }

    pub fn search_function(&self, identifier: &InternedString) -> Option<&FunctionDecl> {
        for entry in &self.functions {
            if entry.identifier == *identifier {
                return Some(entry)
            }
        }
        None
    }
    

    pub fn search_global(& self, identifier: &InternedString) -> Option<&GlobalVarDecl> {
        for entry in &self.globals {
            if entry.identifier == *identifier {
                return Some(entry)
            }
        }
        None
    }

    // TODO: search for structs vs enums vs unions.
    pub fn search_record(& self, identifier: &InternedString) -> Option<&RecordDecl> {
        for record in &self.records {
            if *identifier == record.identifier {
                return Some(record);
            }
        }
        None
    }

    pub fn search_local(&self, scope: ScopeHandle, identifier: &InternedString) -> Option<&LocalVarDecl> {
        for entry in &self.resolve_scope(scope).declarations {
            if entry.identifier == *identifier {
                return Some(&entry)
            }
        }
        
        None
    }

    pub fn search_local_up(& self, scope: ScopeHandle, identifier: &InternedString) -> Option<&LocalVarDecl> {
        // This is so disgustingly messy. 
        type S = ScopeKind;
        let parent = match self.resolve_scope(scope).kind {
            S::Function(parent )=> parent,
            S::File => return None,
            S::Block(parent) => parent,
        };

        match self.search_local_up(parent, identifier) {
            Some(entry) => Some(entry),
            None => self.search_local(scope, identifier),
        }
    }

    
    pub fn add_local_decl(&mut self, scope: ScopeHandle, node: ASTNodeHandle, entry: LocalVarDecl) -> Result<(), AnalysisError> {
        if self.search_local(scope, &entry.identifier).is_some() {
            Err(AnalysisError::AlreadyDeclared(entry.identifier, node))
        }
        else {
            self.scope_arena.get_mut(scope)
                .expect("invalid handle")
                .declarations.push(entry);
            Ok(())
        }
    }

    pub fn add_function_decl(&'a mut self, node: ASTNodeHandle, entry: FunctionDecl) -> Result<(), AnalysisError> {
        if self.search_function(&entry.identifier).is_some() {
            Err(AnalysisError::AlreadyDeclared(entry.identifier, node))
        }
        else {
            self.functions.push(entry);
            //self.uses.insert(node, DeclRef::Function(self.functions.last().unwrap()));
            Ok(())
        }
    }


}


// Display Things
pub(crate) struct LocalVarDeclPrintable<'a> {
    pub(crate) declaration: LocalVarDecl,
    pub(crate) context: &'a Context<'a>
}

impl fmt::Display for LocalVarDeclPrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let decl = &self.declaration;
        let type_info = self.context.resolve_type(&decl.type_info);
        write!(f, "hello")
        //write!(f, "size: {}, offset: {}, type_info: {}, kind: {:?}, global: {}", decl.size, decl.offset, 
        //TypePrintable{data: type_info, context: self.context}, false) //decl.is_global) //self.type_info, self.kind)
    }
}