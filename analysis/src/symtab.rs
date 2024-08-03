use core::fmt;
use std::{ops::Deref, rc::Rc, cell::RefCell};

use lex_parse::{context::{InternedType, InternedString, Context}, ast::ASTNodeHandle, error::AnalysisError, types::TypePrintable};
use slotmap::{SparseSecondaryMap, SlotMap};

slotmap::new_key_type! { pub struct ScopeHandle; }

#[derive(Debug, Clone,  PartialEq, Eq, Hash)]
pub struct Scope {
    pub(crate) parent: Option<ScopeHandle>,
    pub(crate) declarations: Vec<VarDecl>,
    // pub(crate) tags: 
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub identifier: InternedString, 
    pub return_ty: InternedType,
    pub parameters_ty: Vec<InternedType>,
    //TODO: info about parameters for type checking.
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarDecl {
    pub scope: ScopeHandle,
    pub identifier: InternedString, 
    pub size: usize,
    pub type_info: InternedType,
    pub is_parameter: bool,
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
    pub(crate) complete: bool,
    pub(crate) identifier: InternedString, 
    pub(crate) size: usize,
    pub(crate) fields: Vec<FieldDecl>,
}


pub struct SymbolTable {
    // Maps Declarations AND symbol refs to Declarations.
    // No codegen required:
    pub(crate) records: Vec<RecordDecl>, // This is basically a type. // Global Records only

    // Codegen required:
    pub(crate) functions: Vec<FunctionDecl>,

    pub root: Option<ScopeHandle>,

    pub(crate) scopes: SlotMap<ScopeHandle, Scope>,
}


impl <'a> SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {  
            records: Vec::new(), 
            functions: Vec::new(), 
            root: None,
            scopes: SlotMap::with_key(),
        }
    }

    pub fn resolve_scope(&self, scope_h: ScopeHandle) -> &Scope {
        self.scopes.get(scope_h).expect("invalid handle")
    }

    pub fn get_function(&self, identifier: &InternedString) -> Option<&FunctionDecl> {
        for entry in &self.functions {
            if entry.identifier == *identifier {
                return Some(entry)
            }
        }
        None
    }
    
    // TODO: search for structs vs enums vs unions.
    pub fn get_record(& self, identifier: &InternedString) -> Option<&RecordDecl> {
        for record in &self.records {
            if *identifier == record.identifier {
                return Some(record);
            }
        }
        None
    }


    pub fn search_scope(&self, scope: &ScopeHandle, identifier: &InternedString) -> Option<&VarDecl> {
        for entry in &self.resolve_scope(*scope).declarations {
            if entry.identifier == *identifier {
                return Some(&entry)
            }
        }
        
        None
    }

    pub fn search_tree(& self, scope: &ScopeHandle, identifier: &InternedString) -> Option<&VarDecl> {
        match self.search_scope(&scope, identifier) {
            Some(entry) => Some(entry),
            None => {
                let Some(parent) = self.scopes.get(*scope).expect("invalid scope handle").parent else {
                    panic!()
                };
                self.search_tree(&parent, identifier)
            }
        }
    }

    pub fn add_var_decl(&mut self, scope: &ScopeHandle, node: &ASTNodeHandle, entry: VarDecl) -> Result<(), AnalysisError> {
        if self.search_scope(scope, &entry.identifier).is_some() {
            Err(AnalysisError::AlreadyDeclared(entry.identifier, *node))
        }
        else {
            self.scopes.get_mut(*scope)
                .expect("invalid handle")
                .declarations.push(entry);
            Ok(())
        }
    }

    pub fn add_function_decl(&'a mut self, node: ASTNodeHandle, entry: FunctionDecl) -> Result<(), AnalysisError> {
        if self.get_function(&entry.identifier).is_some() {
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
pub(crate) struct VarDeclPrintable<'a> {
    pub(crate) declaration: VarDecl,
    pub(crate) context: &'a Context<'a>
}

impl fmt::Display for VarDeclPrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let decl = &self.declaration;
        let type_info = self.context.resolve_type(&decl.type_info);
        write!(f, "hello")
        //write!(f, "size: {}, offset: {}, type_info: {}, kind: {:?}, global: {}", decl.size, decl.offset, 
        //TypePrintable{data: type_info, context: self.context}, false) //decl.is_global) //self.type_info, self.kind)
    }
}