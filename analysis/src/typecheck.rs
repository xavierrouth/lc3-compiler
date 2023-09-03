use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap, SecondaryMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle, BinaryOpType, ASTNodePrintable, UnaryOpType};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{Context, InternedType, InternedString};
use lex_parse::types::{Type, DeclaratorPart, TypeSpecifier, StorageQual, Qualifiers, CType};

use crate::symbol_table::SymbolTable;


pub struct Typecheck<'a> {
    symbol_table: &'a SymbolTable, // 
    ast: &'a mut AST,

    types: SecondaryMap<ASTNodeHandle, InternedType>,
    values: SecondaryMap<ASTNodeHandle, V>,
    casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,
    halt: bool,

    context: &'a Context<'a> //TODO: Merge error handler and context.
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeCast {
    ArrayToPointerDecay,
    LvalueToRvalue,
}

impl fmt::Display for TypeCast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCast::ArrayToPointerDecay => write!(f, "Array to Pointer Decay"),
            TypeCast::LvalueToRvalue => write!(f, "LValue to Rvalue"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum V {
    LValue,
    RValue,
}

impl <'a> Vistior<'a> for Typecheck<'a> {
    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.get_node(node_h)
    }

    fn postorder(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h).clone();
        // We only need to check expressions really.
        let value = match node {
            ASTNode::IntLiteral { value } => {
                self.types.insert(*node_h, self.get_int_type());
                V::RValue
            },
            ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                self.try_array_decay(node_h); // Need to do this on pointer loads, and lvalue field selections, because these could all be arrays.
                V::LValue
            },
            ASTNode::BinaryOp { op, right, left } => {
                match op {
                    BinaryOpType::Assign => {
                        match self.values.get(right).unwrap() {
                            V::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            V::RValue => ()
                        }
                        // Check here that Rleft is Rvalue.
                    },
                    _ => {
                        // Convert lvalues to Rvalues
                        // Insert Cast Node
                        // Very difficult to mutate data structure you are traversing over.
                        /**
                        let mut binop = self.ast.nodes.get_mut(*node_h);

                        if let Some(binop) = binop.as_mut() {
                            if let ASTNode::BinaryOp { op, right, left } = binop {
                                // Try a cast on both left and right LOL
                                let child = right.clone();
                                let cast = ASTNode::UnaryOp { op: lex_parse::ast::UnaryOpType::Address, child: child, order: false };
                                let cast = self.ast.nodes.insert(cast);
                                *right = cast;
                            }
                        };  */

                        //let cast = 


                        match self.values.get(right).unwrap() {



                            V::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            V::RValue => ()
                        }
                        match self.values.get(left).unwrap() {
                            V::LValue => {self.casts.insert(left, TypeCast::LvalueToRvalue);},
                            V::RValue => ()
                        }
                    }
                }
                V::RValue // Unless its a array index, then you can get an Lvalue from that
                
            },
            _ => {V::RValue},
        };
        self.values.insert(*node_h, value);
    }

    fn halt(& self) -> bool {false}
}

impl <'a, 'ast> Typecheck<'ast> {
    pub fn new(symbol_table: &'a SymbolTable, ast: &'a mut AST, context: &'a Context<'a>, error_handler: &'a ErrorHandler<'a>,) -> Typecheck<'a> {
        Typecheck { symbol_table, ast, types: SecondaryMap::new(), values: SecondaryMap::new(), casts: SparseSecondaryMap::new(), halt: false, context }
    }

    pub fn print_casts(&self) -> () {
        for (handle, cast) in &self.casts {
            let node = self.get_node(&handle);
            match node {
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, cast)}
            }
        }
    } 

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

    fn try_array_decay(& mut self, node_h: &ASTNodeHandle) -> () {
        // Assert that this is a symbol ref.
        let symbol = self.symbol_table.entries.get(*node_h).unwrap();

        if self.context.resolve_type(symbol.type_info).is_array() {
            self.casts.insert(*node_h, TypeCast::ArrayToPointerDecay);

            let mut r#type = self.context.resolve_type(symbol.type_info).clone();
            let first = r#type.declarator.first_mut().unwrap();
            *first = DeclaratorPart::PointerDecl(None);

            let r#type = self.context.get_type(&r#type);
            self.types.insert(*node_h, r#type);

        }        
    }
}

