use core::fmt;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap, SecondaryMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle, ASTNodePrintable, BinaryOpType};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{InternedString, InternedType, Context, self};
use lex_parse::types::{Type, DeclaratorPart, CType};

use crate::symbol_table::{SymbolTable, Scope, LocalVarDecl, RecordDecl, FunctionDecl, FieldDecl, DeclRef, ScopeHandle, ScopeKind, self};


// TODO: 
/*
pub struct AnalysisResult {
    symbol_table: SymbolTable,
}  */

/** Analysis pass on AST, borrows AST while it is alive. 
 *  Populates a symbol table, and maps symbol references to the corresponding declarations in the symbol table.
 *  
*/
pub struct AnalysisPass<'ctx> {
    pub symbol_table: SymbolTable, // 

    curr_scope: ScopeHandle,

    // This is kinda disgusting, but neceesary
    scopes: SparseSecondaryMap<ASTNodeHandle, ScopeHandle>,

    halt: bool,

    ast: &'ctx AST,
    error_handler: &'ctx ErrorHandler<'ctx>,
    context: &'ctx Context<'ctx>, //TODO: Merge error handler and context.
}

impl <'ctx> AnalysisPass<'ctx> {

    pub fn new(ast: &'ctx AST, context: &'ctx Context<'ctx>, error_handler: &'ctx ErrorHandler<'ctx>) -> AnalysisPass<'ctx> {
        let mut symbol_table = SymbolTable::new();
        let file_scope = Scope {kind: ScopeKind::File, declarations: Vec::new()};
        let file_scope = symbol_table.scope_arena.insert(file_scope);
        AnalysisPass { symbol_table, scopes: SparseSecondaryMap::new(), ast, context, error_handler, 
            curr_scope: file_scope, halt: false}
    }

    /* 
    pub fn print_symbol_table(&self) -> () {
        for (handle, declaration) in &self.symbol_table.entries {
            let node = self.get(handle);
            match node {
                ASTNode::SymbolRef{.. } => (),    
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, LocalVarDeclPrintable{declaration: declaration.clone(), context: self.context})}
            }
        }
        println!("Records:");
        for record in &self.symbol_table.records {
            let RecordDecl { identifier, size, fields } = record;
            let identifier = self.context.resolve_string(*identifier);
            println!("<struct {identifier}, size: {size}>");
            for field in fields {
                println!("{}", LocalVarDeclPrintable{declaration: field.clone(), context: self.context});
            }
        }
    }*/
    
    fn report_error(&mut self, error: AnalysisError) -> () {
        self.halt = true;

        self.error_handler.print_analysis_error(error);
        
        ()
    }

}


impl <'ctx> Vistior<'ctx> for AnalysisPass<'ctx> {
    // TODO: Should this really be in preorder??
    fn preorder(& mut self, node_h: ASTNodeHandle) -> () {
        let node = self.get(node_h).clone();
        match node {
            ASTNode::CompoundStmt { statements: _, new_scope } => {
                if new_scope == true {
                    // Copy over offsets, because we stil are on the same stack frame.
                    // TODO: Distinguish between scopes / stack frames?
                    let block_scope = Scope {declarations: Vec::new(), kind: ScopeKind::Block(self.curr_scope.clone())};
                    self.curr_scope = self.symbol_table.scope_arena.insert(block_scope)
                }
            },
            ASTNode::RecordDecl { identifier, record_type, fields } => {
                let mut record_fields = Vec::new();

                let mut record_size: usize = 0;

                // Start generating a record Decl.
                for field in fields {
                    let ASTNode::FieldDecl{identifier, type_info} = self.get(field) else {
                        return;
                    };
                    // There can be an error here, infinite size struct.
                    let size = self.context.resolve_type(type_info).calculate_size();
                    
                    let decl = FieldDecl {
                        identifier: *identifier,
                        size,
                        type_info: *type_info,
                    };
                    record_size += size;
                    record_fields.push(decl);
                }
                
                self.symbol_table.records.push(
                RecordDecl {
                    identifier,
                    fields: record_fields,
                    complete: true,
                    size: record_size,
                });

                
            },
            ASTNode::VariableDecl { identifier, initializer: _, type_info } => {
                // Make a new entry
                // We should really store both Records and Types in the type contaxt.
                let size = {
                    let type_info = self.context.resolve_type(&type_info);
                    if type_info.is_record() {
                        let Some(CType::Struct(type_name)) = type_info.specifier.ctype else {
                            self.report_error(AnalysisError::General(node_h));
                            return;
                        };
                        let Some(record) = self.symbol_table.search_record(&type_name) else {
                            self.report_error(AnalysisError::General(node_h));
                            return;
                        };
                        record.size

                    }
                    else {
                        type_info.calculate_size()
                    }
                };
                // Derive size from TypeInfo


                let entry = LocalVarDecl {
                    scope: self.curr_scope, // Need this for hashing!
                    identifier,
                    size,
                    type_info,
                    is_parameter: false,
                };
                

                // Need some way to error out here:
                // How am i supposed to extract error from eresuklt?
                              
                match self.symbol_table.add_local_decl(self.curr_scope.clone(), node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }  

            },
            ASTNode::ParameterDecl { identifier, type_info } => {
                // Make a new entry
                let size = {
                    let type_info = self.context.resolve_type(&type_info);
                    if type_info.is_record() {
                        let Some(CType::Struct(type_name)) = type_info.specifier.ctype else {
                            self.report_error(AnalysisError::General(node_h));
                            return;
                        };
                        let Some(record) = self.symbol_table.search_record(&type_name) else {
                            self.report_error(AnalysisError::General(node_h));
                            return;
                        };
                        record.size

                    }
                    else {
                        type_info.calculate_size()
                    }
                };
                
                

                // ASsert that size = 1
                let entry = LocalVarDecl {
                    scope: self.curr_scope, // Need this for hashing!
                    identifier,
                    size,
                    is_parameter: true,
                    type_info,
                };

                let scope = self.curr_scope.clone();

                // Need some way to error out here:
                match self.symbol_table.add_local_decl(scope, node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }
            },
            ASTNode::FunctionDecl { body: _, parameters: _, identifier, return_type } => {
                let entry = FunctionDecl {
                    identifier: identifier,
                    type_info: r#return_type,
                };

                // Need some way to error out here:
                match self.symbol_table.add_function_decl(node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                };

                // TODO: Distinguish between scopes / stack frames?
                
                let function_scope = Scope {declarations: Vec::new(), kind: ScopeKind::Function(self.curr_scope.clone())};
                self.curr_scope = self.symbol_table.scope_arena.insert(function_scope)
            }
            ASTNode::ForStmt { initializer: _, condition: _, update: _, body: _ } => {
                // TODO: Distinguish between scopes / stack frames?
                let block_scope = Scope {declarations: Vec::new(), kind: ScopeKind::Block(self.curr_scope.clone())};
                self.curr_scope = self.symbol_table.scope_arena.insert(block_scope)
            }
            // Not a delcaration:
            ASTNode::SymbolRef { identifier } => {
                // If this node already has an entry, than just ignore it:,
                // This will happen if FunctionCall sets it, or a binop that is a struct access.
                /*
                if self.uses.get(node_h).is_some() {
                    return;
                }

                // Make sure that it exists, and then map this node to the existing entry.
                match self.symbol_table.search_local_up(&identifier) {
                    Some(entry) => {
                        self.uses.insert(node_h, DeclRef::LocalVar(entry.clone())); // Map node to the entry information that it needs
                    },
                    None => {
                        let error = AnalysisError::UnknownSymbol(identifier, node_h);
                        self.report_error(error);
                    }
                }  */
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                // Need to do make sure this is a valid function, .i.e search up scope for it's symbol.
                // Make sure that it exists, and then map this node to the existing entry.
                /* 
                let child = self.get(symbol_ref).clone();

                let (entry, identifier) = match child {
                    ASTNode::SymbolRef { identifier } => {
                        (self.symbol_table.search_function(&identifier), identifier)
                    }
                    _ => {
                        return;
                    }
                };

                match entry {
                    Some(entry) => {
                        self.uses.insert(symbol_ref, DeclRef::Function(entry.clone()));
                    },
                    None => {
                        let error = AnalysisError::UnknownSymbol(identifier, node_h);
                        self.report_error(error);
                    }
                } */
            }
            _ => ()
        }
    }

    fn postorder(&mut self, node_h: ASTNodeHandle) -> () {
        let node = self.get(node_h);
        match node {
            ASTNode::CompoundStmt { statements: _, new_scope } => {
                if *new_scope {
                     match self.symbol_table.resolve_scope(self.curr_scope) {
                        Scope{kind: ScopeKind::Block(parent), ..} => {
                            self.curr_scope = parent.clone()
                        }
                        Scope{kind: ScopeKind::Function(parent), ..} => {
                            self.curr_scope = parent.clone()
                        }
                        _ => panic!("Uh oh!")
                    };
                }
                
            }
            ASTNode::FunctionDecl { body: _, parameters: _, identifier: _, return_type: _ } => {
                match self.symbol_table.resolve_scope(self.curr_scope) {
                    Scope{kind: ScopeKind::Block(parent), ..} => {
                        self.curr_scope = parent.clone();
                    }
                    Scope{kind: ScopeKind::Function(parent), ..} => {
                        self.curr_scope = parent.clone();
                    }
                    _ => panic!("Uh oh!")
                };
            }
            _ => ()
        }
    }

    fn get(&self, node_h: ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(node_h).unwrap()
    }

    fn halt(&self) -> bool {
        self.halt
    }

}

 