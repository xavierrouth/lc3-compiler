use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap};
use string_interner::symbol::{self, SymbolU16};
use crate::ast::{Vistior, AST, ASTNode, ASTNodeHandle, TraversalOrder};
use crate::error::ErrorHandler;
use crate::strings::InternedString;

use crate::types::TypeInfo;

#[derive(Debug)]
pub enum AnalysisError {
    AlreadyDeclared(InternedString, ASTNodeHandle), // Used to extract line information. 
    UnknownSymbol(InternedString, ASTNodeHandle),
}

pub struct STScope {
    next_param_slot: i32,
    next_variable_slot: i32,
    var_entries: Vec<STEntry>,
    pub is_global: bool,
    // tag_entries: Vec<STEntry>, TODO
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
pub enum STEntryType {
    VarOrParam,
    Function,
    Tag,
    Label,
}

#[derive(Debug, Clone, PartialEq)]
pub struct STEntry {
    pub identifier: InternedString, // TODO Make this a string
    pub size: usize,
    pub offset: i32,
    pub type_info: TypeInfo,
    pub is_global: bool,
    kind: STEntryType
} 

impl fmt::Display for STEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "size: {}, offset: {}, type_info: {}, kind: {:?}, global: {}", self.size, self.offset, "none", self.kind, self.is_global) //self.type_info, self.kind)
    }
}

pub struct SymbolTable {
    pub entries: SparseSecondaryMap<ASTNodeHandle, STEntry>,
    stack: Vec<STScope>, // Reference to a STScope
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut st = SymbolTable { entries: SparseSecondaryMap::new(), stack: Vec::new()};
        let mut global_scope = STScope::new();
        global_scope.is_global = true;
        st.stack.push(global_scope);
        st
    }
    pub fn search_scope(&mut self, identifier: &InternedString, entry_type: &STEntryType) -> Option<STEntry> {

        for entry in self.stack.last().unwrap().var_entries.as_slice() {
            if (entry.identifier == *identifier) && (*entry_type == entry.kind) {
                return Some(entry.clone())
            }
        }
        None
    } 

    pub fn search_up(&mut self, identifier: &InternedString, entry_type: &STEntryType) -> Option<STEntry> {
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

    pub fn add(&mut self, node: ASTNodeHandle, entry: STEntry) -> Result<(), AnalysisError> {
        if self.search_scope(&entry.identifier, &entry.kind).is_some() {
            Err(AnalysisError::AlreadyDeclared(entry.identifier, node))
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
    pub symbol_table: SymbolTable, // 
    ast: &'a AST,
    error_handler: Rc<RefCell<ErrorHandler>>,
    halt: bool,
}

impl <'a> Analyzer<'a> {
    pub fn new(ast: &'a AST, error_handler: Rc<RefCell<ErrorHandler>>,) -> Analyzer<'a> {
        Analyzer { symbol_table: SymbolTable::new(), ast: ast, error_handler, halt: false}
    }

    fn enter_scope(&mut self, _next_param_slot: i32, _next_variable_slot: i32) -> () {
        self.symbol_table.stack.push(STScope::new());
    }

    fn curr_scope(&mut self) -> &mut STScope {
        self.symbol_table.stack.last_mut().unwrap()
    }

    fn exit_scope(&mut self) -> () {
        self.symbol_table.stack.pop();
        ()
    }

    pub fn print_symbol_table(&self) -> () {

        for (key, value) in &self.symbol_table.entries {
            let node = self.get_node(&key);
            println!("{} {}", node, value );
        }
    }

    fn report_error(&mut self, error: AnalysisError) -> () {
        self.error_handler.borrow_mut().print_analysis_error(error);
        self.halt = true;
    }

    
}

impl <'a> Vistior<'a> for Analyzer<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn entry(&mut self, _node_h: &ASTNodeHandle) -> () {
       
    }

    fn exit(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h);
        match node {
            ASTNode::CompoundStmt { statements: _, new_scope } => {
                if *new_scope {self.exit_scope();}
            }
            ASTNode::FunctionDecl { body: _, parameters: _, identifier: _, return_type: _ } => {
                self.exit_scope();
            }
            
            _ => ()
        }
    }

    fn operate(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h).clone();
        match node {
            ASTNode::CompoundStmt { statements: _, new_scope } => {
                if new_scope == true {
                    // Copy over offsets, because we stil are on the same stack frame.
                    let next_param_slot = self.curr_scope().next_param_slot;
                    let next_variable_slot = self.curr_scope().next_variable_slot;
                    // TODO: Distinguish between scopes / stack frames?
                    self.enter_scope(next_param_slot, next_variable_slot);
                }
            },
            ASTNode::VariableDecl { identifier, initializer: _, type_info } => {
                // Make a new entry
                
                // Derive size from TypeInfo
                let scope = self.curr_scope();

                let size = type_info.calculate_size();

                let entry = STEntry {
                    identifier: identifier,
                    size: size,
                    offset: scope.next_variable_slot * -1,
                    kind: STEntryType::VarOrParam,
                    type_info: type_info,
                    is_global: scope.is_global,
                };
                scope.next_variable_slot += size as i32;

                // Need some way to error out here:
                // How am i supposed to extract error from eresuklt?
                match self.symbol_table.add(*node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }
            },
            ASTNode::ParameterDecl { identifier, type_info } => {
                // Make a new entry
                
                let scope = self.curr_scope();

                // ASsert that size = 1
                let entry = STEntry {
                    identifier: identifier,
                    size: 1,
                    offset: scope.next_param_slot + 4,
                    kind: STEntryType::VarOrParam,
                    type_info: type_info,
                    is_global: false,
                };
                scope.next_param_slot += 1;

                // Need some way to error out here:
                match self.symbol_table.add(*node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }
            }
            ASTNode::FunctionDecl { body: _, parameters: _, identifier, return_type } => {
                let entry = STEntry {
                    identifier: identifier,
                    size: 1,
                    offset: 0,
                    kind: STEntryType::Function,
                    type_info: r#return_type,
                    is_global: true,
                };

                // Need some way to error out here:
                match self.symbol_table.add(*node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                };

                // TODO: Distinguish between scopes / stack frames?
                self.enter_scope(0, 0);
            }
            ASTNode::ForStmt { initializer: _, condition: _, update: _, body: _ } => {
                let next_param_slot = self.curr_scope().next_param_slot;
                let next_variable_slot = self.curr_scope().next_variable_slot;
                // TODO: Distinguish between scopes / stack frames?
                self.enter_scope(next_param_slot, next_variable_slot);
            }
            // Not a delcaration:
            ASTNode::SymbolRef { identifier } => {
                // If this node already has an entry, than just ignore it:
                if (self.symbol_table.entries.get(*node_h).is_some()) {
                    return;
                }

                // Make sure that it exists, and then map this node to the existing entry.
                let entry = self.symbol_table.search_up(&identifier, &STEntryType::VarOrParam);
                if entry.is_none() {
                    let error = AnalysisError::UnknownSymbol(identifier, *node_h);
                    self.report_error(error);
                }
                else {
                    self.symbol_table.entries.insert(*node_h, entry.unwrap()); // Map node to the entry information that it needs
                }
                
            }
            ASTNode::FunctionCall { symbol_ref: symbol_ref, arguments: arguments } => {
                // Need to do make sure this is a valid function, .i.e search up scope for it's symbol.
                // Make sure that it exists, and then map this node to the existing entry.
                let child = self.get_node(&symbol_ref).clone();

                let (entry, identifier) = match child {
                    ASTNode::SymbolRef { identifier } => {
                        (self.symbol_table.search_up(&identifier, &STEntryType::Function), identifier)
                    }
                    _ => {
                        return;
                    }
                };

                if entry.is_none() {
                    let error = AnalysisError::UnknownSymbol(identifier, *node_h);
                    self.report_error(error);
                }
                else {
                    self.symbol_table.entries.insert(symbol_ref, entry.unwrap());
                }
            }
            _ => ()
        }
    }

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }

    fn halt(&self) -> bool {
        self.halt
    }

}

 