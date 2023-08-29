

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use slotmap::{SlotMap, SparseSecondaryMap, SecondaryMap};
use crate::ast::{Vistior, AST, ASTNode, ASTNodeHandle, TraversalOrder};
use crate::strings::InternedString;
use crate::token::{Token, TokenKind};
use crate::types::TypeInfo;

#[derive(Debug)]
pub enum AnalysisError {
    AlreadyDeclared
}

pub struct STScope {
    next_param_slot: i32,
    next_variable_slot: i32,
    var_entries: Vec<STEntry>,
    // tag_entries: Vec<STEntry>, TODO
    // label_entries: TODO
}

impl STScope {
    pub fn new() -> STScope {
        STScope {
            next_param_slot: 0,
            next_variable_slot: 0,
            var_entries: Vec::new(),
        }
    }

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum STEntryType {
    VarOrParam,
    Function,
    Tag,
    Label,
}

#[derive(Debug, Clone, PartialEq)]
pub struct STEntry {
    identifier: InternedString, // TODO Make this a string
    size: usize,
    offset: i32,
    type_info: TypeInfo,
    kind: STEntryType
} 

pub struct SymbolTable {
    entries: SparseSecondaryMap<ASTNodeHandle, STEntry>,
    stack: Vec<STScope>, // Reference to a STScope
}

impl SymbolTable {
    pub fn search_scope(&mut self, identifier: &InternedString, entry_type: &STEntryType) -> Option<STEntry> {

        for entry in self.stack.last().unwrap().var_entries.as_slice() {
            if entry.identifier == *identifier {
                return Some(entry.clone())
            }
        }
        None
    } 

    pub fn search_up(&mut self, identifier: &InternedString, entry_type: &STEntryType) -> Option<STEntry> {

        let entry = self.search_scope(&identifier, &entry_type);

        for scope in self.stack.iter().rev(){
            for entry in scope.var_entries.as_slice() {
                return Some(entry.clone())
            }
        }
        entry
    }

    pub fn add(&mut self, node: ASTNodeHandle, entry: STEntry) -> Result<(), AnalysisError> {
        if self.search_scope(&entry.identifier, &entry.kind).is_some() {
            Err(AnalysisError::AlreadyDeclared)
        }
        else {
            self.stack.last_mut().unwrap().var_entries.push(entry.clone());
            self.entries.insert(node, entry);
            Ok(())
        }
    }
}

/** Analysis pass on AST, borrows AST while it is alive. */
pub struct Analyzer<'a> {
    symbol_table: SymbolTable, // 
    ast: &'a AST,
}

impl <'a> Analyzer<'a> {
    pub fn new(ast: &'a AST) -> Analyzer<'a> {
        Analyzer { symbol_table: SymbolTable {entries: SparseSecondaryMap::new(), stack: Vec::new()}, ast: ast }
    }

    fn enter_scope(&mut self, next_param_slot: i32, next_variable_slot: i32) -> () {
        self.symbol_table.stack.push(STScope::new());
    }

    fn curr_scope(&mut self) -> &mut STScope {
        self.symbol_table.stack.last_mut().unwrap()
    }

    fn exit_scope(&mut self) -> () {
        self.symbol_table.stack.pop();
        ()
    }

    
}

impl <'a> Vistior<'a> for Analyzer<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn entry(&mut self, node_h: &ASTNodeHandle) -> () {
       
    }

    fn exit(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h);
        match node {
            ASTNode::CompoundStmt { statements, new_scope } => {
                if *new_scope {self.exit_scope();}
            }
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                self.exit_scope();
            }
            
            _ => ()
        }
    }

    fn operate(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h);
        match node {
            ASTNode::CompoundStmt { statements, new_scope } => {
                if *new_scope == true {
                    // Copy over offsets, because we stil are on the same stack frame.
                    let next_param_slot = self.curr_scope().next_param_slot;
                    let next_variable_slot = self.curr_scope().next_variable_slot;
                    // TODO: Distinguish between scopes / stack frames?
                    self.enter_scope(next_param_slot, next_variable_slot);
                }
            },
            ASTNode::VariableDecl { identifier, initializer, r#type } => {
                // Make a new entry
                
                let scope = self.curr_scope();

                let entry = STEntry {
                    identifier: *identifier,
                    size: 1,
                    offset: scope.next_variable_slot * -1,
                    kind: STEntryType::VarOrParam,
                    type_info: *r#type,
                };
                scope.next_variable_slot += 1;

                // Need some way to error out here:
                self.symbol_table.add(*node_h, entry);
            },
            ASTNode::ParameterDecl { identifier, r#type } => {
                // Make a new entry
                
                let scope = self.curr_scope();

                let entry = STEntry {
                    identifier: *identifier,
                    size: 1,
                    offset: scope.next_param_slot + 4,
                    kind: STEntryType::VarOrParam,
                    type_info: *r#type,
                };
                scope.next_param_slot += 1;

                // Need some way to error out here:
                self.symbol_table.add(*node_h, entry);
            }
            ASTNode::FunctionDecl { body: _, parameters: _, identifier, return_type } => {
                let entry = STEntry {
                    identifier: *identifier,
                    size: 1,
                    offset: 0,
                    kind: STEntryType::VarOrParam,
                    type_info: *r#return_type,
                };

                // Need some way to error out here:
                self.symbol_table.add(*node_h, entry);
            }
            ASTNode::ForStmt { initializer, condition, update, body } => {
                let next_param_slot = self.curr_scope().next_param_slot;
                let next_variable_slot = self.curr_scope().next_variable_slot;
                // TODO: Distinguish between scopes / stack frames?
                self.enter_scope(next_param_slot, next_variable_slot);
            }
            // Not a delcaration:
            ASTNode::SymbolRef { identifier } => {
                // Make sure that it exists, and then map this node to the existing entry.
                let entry = self.symbol_table.search_up(&identifier, &STEntryType::VarOrParam);
                if entry.is_none() {
                    // Error here.
                }
                else {
                    self.symbol_table.entries.insert(*node_h, entry.unwrap()); // Map node to the entry information that it needs
                }
                
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                // Need to do make sure this is a valid function, .i.e search up scope for it's symbol.
                // Oh well, we can just do it later, otherwise we need to explicitly state that we have already handled this node and its children, and not call operate on the child reference.
            }
            _ => ()
        }
    }

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }

}
