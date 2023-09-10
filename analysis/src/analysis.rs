use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap, SecondaryMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle, ASTNodePrintable, BinaryOpType};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{InternedString, InternedType, Context, self};
use lex_parse::types::{Type, DeclaratorPart, CType};

use crate::symbol_table::{self, SymbolTable, Scope, Declaration, DeclarationType, DeclarationPrintable, Record};

/** Analysis pass on AST, borrows AST while it is alive. 
 *  Populates a symbol table, and maps symbol references to the corresponding declarations in the symbol table.
 *  
*/
pub struct Analyzer<'a> {
    pub symbol_table: SymbolTable, // 
    halt: bool,

    ast: &'a AST,
    error_handler: &'a ErrorHandler<'a>,
    context: &'a Context<'a>, //TODO: Merge error handler and context.
}

impl <'a> Analyzer<'a> {
    pub fn new(ast: &'a AST, context: &'a Context<'a>, error_handler: &'a ErrorHandler<'a>) -> Analyzer<'a> {
        Analyzer { symbol_table: SymbolTable::new(), ast, context, error_handler, halt: false}
    }

    pub fn print_symbol_table(&self) -> () {
        for (handle, declaration) in &self.symbol_table.entries {
            let node = self.get_node(&handle);
            match node {
                ASTNode::SymbolRef{.. } => (),    
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, DeclarationPrintable{declaration: declaration.clone(), context: self.context})}
            }
        }
        println!("Records:");
        for record in &self.symbol_table.records {
            let Record { identifier, size, fields } = record;
            let identifier = self.context.resolve_string(*identifier);
            println!("<struct {identifier}, size: {size}>");
            for field in fields {
                println!("{}", DeclarationPrintable{declaration: field.clone(), context: self.context});
            }
        }
    }

    fn enter_scope(&mut self, next_param_slot: i32, next_variable_slot: i32) -> () {
        self.symbol_table.stack.push(Scope::new(next_param_slot, next_variable_slot));
    }

    fn curr_scope(&mut self) -> &mut Scope {
        self.symbol_table.stack.last_mut().unwrap()
    }

    fn exit_scope(&mut self) -> () {
        self.symbol_table.stack.pop();
        ()
    }

    
    fn report_error(&mut self, error: AnalysisError) -> () {
        self.halt = true;

        self.error_handler.print_analysis_error(error);
        
        ()
    }

}

impl <'a> Vistior<'a> for Analyzer<'a> {
    // TODO: Should this really be in preorder??
    fn preorder(&mut self, node_h: &ASTNodeHandle) -> () {
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
            ASTNode::RecordDecl { identifier, record_type, fields } => {
                let mut record_fields = Vec::new();
                let mut offset: usize = 0;

                for field in fields {
                    let ASTNode::FieldDecl{identifier, type_info} = self.get_node(&field) else {
                        return;
                    };
                    let size = self.context.resolve_type(*type_info).calculate_size(); 
                    let decl = Declaration {
                        identifier: *identifier,
                        size: size,
                        offset: offset.try_into().unwrap(),
                        type_info: *type_info,
                        is_global: false,
                        kind: DeclarationType::Field,
                    };
                    offset += size;
                    record_fields.push(decl);
                }
                
                self.symbol_table.records.push(
                Record {
                    identifier,
                    size: offset,
                    fields: record_fields,
                });

                // Start generating a record Decl.
            },
            ASTNode::VariableDecl { identifier, initializer: _, type_info } => {
                // Make a new entry
                // We should really store both Records and Types in the type contaxt.
                let size = {
                    let type_info = self.context.resolve_type(type_info);
                    if type_info.is_record() {
                        let Some(CType::Struct(type_name)) = type_info.specifier.ctype else {
                            self.report_error(AnalysisError::General(*node_h));
                            return;
                        };
                        let Some(record) = self.symbol_table.search_record(&type_name) else {
                            self.report_error(AnalysisError::General(*node_h));
                            return;
                        };
                        record.size

                    }
                    else {
                        type_info.calculate_size()
                    }
                };
                // Derive size from TypeInfo
                let scope = self.curr_scope();

                scope.next_variable_slot += size as i32;

                let entry = Declaration {
                    identifier,
                    size,
                    offset: (scope.next_variable_slot - 1) * -1,
                    kind: DeclarationType::Var,
                    type_info,
                    is_global: scope.is_global,
                };
                

                // Need some way to error out here:
                // How am i supposed to extract error from eresuklt?
                              
                match self.symbol_table.add(*node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }  

            },
            ASTNode::ParameterDecl { identifier, type_info } => {
                // Make a new entry
                let size = {
                    let type_info = self.context.resolve_type(type_info);
                    if type_info.is_record() {
                        let Some(CType::Struct(type_name)) = type_info.specifier.ctype else {
                            self.report_error(AnalysisError::General(*node_h));
                            return;
                        };
                        let Some(record) = self.symbol_table.search_record(&type_name) else {
                            self.report_error(AnalysisError::General(*node_h));
                            return;
                        };
                        record.size

                    }
                    else {
                        type_info.calculate_size()
                    }
                };
                
                let scope = self.curr_scope();

                // ASsert that size = 1
                let entry = Declaration {
                    identifier: identifier,
                    size: 1,
                    offset: scope.next_param_slot + 4,
                    kind: DeclarationType::Param,
                    type_info: type_info,
                    is_global: false,
                };
                scope.next_param_slot += 1;

                // Need some way to error out here:
                match self.symbol_table.add(*node_h, entry) {
                    Err(error) => {self.report_error(error)}
                    Ok(()) => ()
                }
            },
            ASTNode::FunctionDecl { body: _, parameters: _, identifier, return_type } => {
                let entry = Declaration {
                    identifier: identifier,
                    size: 1,
                    offset: 0,
                    kind: DeclarationType::Function,
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
                // If this node already has an entry, than just ignore it:,
                // This will happen if FunctionCall sets it, or a binop that is a struct access.
                if self.symbol_table.entries.get(*node_h).is_some() {
                    return;
                }

                // Make sure that it exists, and then map this node to the existing entry.
                let entry = self.symbol_table.search_up(&identifier, &DeclarationType::Var);
                if entry.is_none() {
                    let error = AnalysisError::UnknownSymbol(identifier, *node_h);
                    self.report_error(error);
                }
                else {
                    self.symbol_table.entries.insert(*node_h, entry.unwrap()); // Map node to the entry information that it needs
                }
                
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                // Need to do make sure this is a valid function, .i.e search up scope for it's symbol.
                // Make sure that it exists, and then map this node to the existing entry.
                let child = self.get_node(&symbol_ref).clone();

                let (entry, identifier) = match child {
                    ASTNode::SymbolRef { identifier } => {
                        (self.symbol_table.search_up(&identifier, &DeclarationType::Function), identifier)
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

    fn postorder(&mut self, node_h: &ASTNodeHandle) -> () {
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

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }

    fn halt(&self) -> bool {
        self.halt
    }

}

 