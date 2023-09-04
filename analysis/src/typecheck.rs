use core::fmt;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use slotmap::{SparseSecondaryMap, SecondaryMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle, BinaryOpType, ASTNodePrintable, UnaryOpType};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{Context, InternedType, InternedString};
use lex_parse::types::{Type, DeclaratorPart, TypeSpecifier, StorageQual, Qualifiers, CType};

use crate::symbol_table::SymbolTable;


pub struct Typecheck<'a> {
    symbol_table: &'a SymbolTable, // 
    ast: &'a AST,

    pub types: SecondaryMap<ASTNodeHandle, InternedType>,
    pub lr: SecondaryMap<ASTNodeHandle, LR>,
    pub casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,
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
            TypeCast::ArrayToPointerDecay => write!(f, "<Array to Pointer Decay>"),
            TypeCast::LvalueToRvalue => write!(f, "<LValue to RValue>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LR {
    LValue,
    RValue,
}

impl Display for LR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LR::LValue => write!(f, "lvalue"),
            LR::RValue => write!(f, ""),//write!(f, "rvalue"),
        }
    }
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
                LR::RValue
            },
            //ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                self.try_array_decay(node_h); // Need to do this on pointer loads, and lvalue field selections, because these could all be arrays.
                LR::LValue
            },
            ASTNode::VariableDecl { identifier, initializer, type_info } => {
                // If there is an intiializer and initializer is not an rvalue, then cast it to an rvlaue., then, convert to 
                if let Some(initializer) = initializer {
                    match self.lr.get(initializer).unwrap() {
                        LR::LValue => {self.casts.insert(initializer, TypeCast::LvalueToRvalue);},
                        LR::RValue => {},
                    };
                }
                LR::RValue
            }
            ASTNode::UnaryOp { op, child, order } => {
                match op {
                    UnaryOpType::Address => {
                        // TODO: Make sure child is an Lvalue.
                        if self.lr.get(child) != Some(&LR::LValue) {
                            println!("NO");
                            self.halt = true;
                        }
                        // This converts an L-Value to an R-Value.
                        LR::RValue
                    },
                    UnaryOpType::Dereference => {
                        // This converts R-Value to an L-Value.
                        // Apply 
                        // If child is not an Rvalue, then cast child to Rvalue,
                        match self.lr.get(child).unwrap() {
                            LR::LValue => {self.casts.insert(child, TypeCast::LvalueToRvalue);},
                            LR::RValue => {},
                        };
                        // Then Set output as Lvalue.
                        // Cast child to Rvalue: 
                        // (- load from symbol,
                        //  - )
                        LR::LValue
                    },
                    _ => {
                        match self.lr.get(child).unwrap() {
                            LR::LValue => {self.casts.insert(child, TypeCast::LvalueToRvalue);},
                            LR::RValue => {},
                        };
                        LR::RValue
                    }
                }
            },
            ASTNode::BinaryOp { op, right, left } => {
                match op {
                    BinaryOpType::Assign => {
                        match self.lr.get(right).unwrap() {
                            LR::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        // Check here that Rleft is Rvalue.
                    },
                    _ => {
                        // Convert lvalues to Rvalues
                        match self.lr.get(right).unwrap() {
                            LR::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        match self.lr.get(left).unwrap() {
                            LR::LValue => {self.casts.insert(left, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                    }
                }
                LR::RValue // Unless its a array index, then you can get an Lvalue from that
            },
            ASTNode::ReturnStmt { expression } => {
                if let Some(expression) = expression {
                    match self.lr.get(expression).unwrap() {
                        LR::LValue => {self.casts.insert(expression, TypeCast::LvalueToRvalue);},
                        LR::RValue => ()
                    }
                }
                LR::RValue // This shouldn't matter
            }
            _ => LR::RValue,
        };
        self.lr.insert(*node_h, value);
    }

    fn halt(& self) -> bool {false}
}

impl <'a, 'ast> Typecheck<'ast> {
    pub fn new(symbol_table: &'a SymbolTable, ast: &'a AST, context: &'a Context<'a>, error_handler: &'a ErrorHandler<'a>,) -> Typecheck<'a> {
        Typecheck { symbol_table, ast, types: SecondaryMap::new(), lr: SecondaryMap::new(), casts: SparseSecondaryMap::new(), halt: false, context }
    }

    pub fn print_casts(&self) -> () {
        for (handle, cast) in &self.casts {
            let node = self.get_node(&handle);
            match node {
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, cast)}
            }
        }
    } 

    pub fn print_lr(&self) -> () {
        // TODO: Make this print and annotate the AST.
        for (handle, lr) in &self.lr {
            let node = self.get_node(&handle);
            match node {
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, lr)}
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


pub struct TypedASTPrint<'a> {
    pub debug_mode: bool,
    pub depth: usize,
    ast: &'a AST,
    context: &'a Context<'a>,

    types: SecondaryMap<ASTNodeHandle, InternedType>,
    lr: SecondaryMap<ASTNodeHandle, LR>,
    casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,
}

impl <'a> TypedASTPrint<'a> {
    pub fn new(debug_mode: bool, ast: &'a AST, context: &'a Context<'a>, 
        types: SecondaryMap<ASTNodeHandle, InternedType>, lr: SecondaryMap<ASTNodeHandle, LR>, casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,) -> TypedASTPrint<'a> {
        TypedASTPrint { context, debug_mode , depth: 0, ast, types, lr, casts }
    }
}

impl <'a> Vistior<'a> for TypedASTPrint<'a> {

    fn preorder(&mut self, node_h: &ASTNodeHandle) -> () {
        self.depth += 1;
        // TODO: Condition on debug mdoe vs pretty mode
        let node = self.get_node(node_h);
        let whitespace_string: String = std::iter::repeat(' ').take((self.depth - 1) * 4).collect();

        let type_string = match self.types.get(*node_h) {
            Some(t) => format!("{}", self.context.resolve_type(*t)),
            None => "".to_string(),
        };

        let lr = match self.lr.get(*node_h) {
            Some(lr) => format!("{}", lr),
            None => "".to_string(),
        };

        let cast = match self.casts.get(*node_h) {
            Some(cast) => format!("{}", cast),
            None => "".to_string(),
        };

        if self.debug_mode {
            println!("{whitespace_string}{:?}", node)
        }
        else {
            let printable = ASTNodePrintable{ node: node.clone(), context: self.context};
            println!("{whitespace_string}{:} {type_string} {lr} {cast}", printable, );
        }
        
    }

    fn postorder(&mut self, _node_h: &ASTNodeHandle) -> () {
        self.depth -= 1;
    }

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }
    
}