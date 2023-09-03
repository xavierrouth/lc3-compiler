use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap, SecondaryMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{Context, InternedType, InternedString};
use lex_parse::types::{Type, DeclaratorPart, TypeSpecifier, StorageQual, Qualifiers, CType};

use crate::analysis::SymbolTable;


pub struct Typecheck<'a> {
    symbol_table: &'a SymbolTable, // 
    ast: &'a AST,

    types: SecondaryMap<ASTNodeHandle, (InternedType, V)>,
    casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,
    halt: bool,

    context: &'a Context<'a> //TODO: Merge error handler and context.
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast {
    ArrayToPointerDecay,
    LvalueToRvalue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum V {
    LValue,
    RValue,
}

impl <'a, 'ast> Typecheck<'ast> {
    //fn check_ast(&mut self) -> (SecondaryMap<ASTNodeHandle, (TypeInfo, V), SparseSecondaryMap<>) {
        
    //}

    fn get_int_type(&self) -> InternedType {
        self.context.get_type(
            &Type {
                declarator: Vec::new(),
                specifier: TypeSpecifier {
                    qualifiers: Qualifiers { cv: None, storage: StorageQual::Auto},
                    ctype: Some(CType::Int)
                }
            }
        )
    }

    fn check_expression(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h);
        // We only need to check expressions really.
        match node {
            
            ASTNode::IntLiteral { value } => {
                self.types.insert(*node_h, (self.get_int_type(), V::RValue));
            },
            ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                self.try_array_decay(node_h); // Need to do this on pointer loads, and lvalue field selections, because these could all be arrays.
            },
            ASTNode::BinaryOp { op, right, left } => {
                self.check_expression(right);
                self.check_expression(left);
            },
            ASTNode::UnaryOp { op, child, order } => todo!(),
            ASTNode::Ternary { first, second, third } => todo!(),
            _ => (),

        }
    }

    fn try_array_decay(& self, node_h: &ASTNodeHandle) -> () {
        // Assert that this is a symbol ref.
        let symbol = self.symbol_table.entries.get(*node_h).unwrap();

        if self.context.resolve_type(symbol.type_info).is_array() {
            self.casts.insert(*node_h, TypeCast::ArrayToPointerDecay);

            let mut r#type = self.context.resolve_type(symbol.type_info).clone();
            let first = r#type.declarator.first_mut().unwrap();
            *first = DeclaratorPart::PointerDecl(None);

            let r#type = self.context.get_type(&r#type);
            self.types.insert(*node_h, (r#type, V::RValue));
        }        
    }

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }


}
/*

ASTNode::SymbolRef { identifier } => {
    self.try_array_decay(node_h);
}
ASTNode::IntLiteral { value } => {
    self.mutate_entry(node_h, |entry| {
        entry.type_info.lr = Some(V::Rvalue);
    });
}

_ => (),


    // This seems so wrong.
    fn try_array_decay(&mut self, node_h: &ASTNodeHandle) -> () {
        let tmp = self.symbol_table.entries.get_mut(*node_h);
        let mut entry = tmp;

        if let Some(entry) = entry.as_mut() {
            if entry.type_info.lr == Some(V::Lvalue) && entry.type_info.is_array() {
                let first = entry.type_info.declarator.first_mut().unwrap();
                *first = DeclaratorPart::PointerDecl(None);
                entry.type_info.lr = Some(V::Rvalue);
            }
        }
    }

    fn mutate_entry<F>(&mut self, node_h: &ASTNodeHandle, closure: F)
    where F: Fn(&mut &mut Declaration),
    {
        let tmp = self.symbol_table.entries.get_mut(*node_h);
        let mut entry = tmp;

        if let Some(entry) = entry.as_mut() {
            closure(entry);
        }
        else {
            let entry = Declaration {

            }
        }
    } */