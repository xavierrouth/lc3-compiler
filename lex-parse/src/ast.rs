
use slotmap::{SlotMap, SecondaryMap};
use std::fmt;
use std::mem::Discriminant;

use crate::types::{TypeInfo};
use crate::token::{Token, TokenKind};

// Need to maintain some maps, first is debug info, which maps ASTNodes to tokens.

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    BitAnd,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    NotEqual,
    EqualEqual,
    Assign,
}
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpType {
    Increment,
    Decrement,
    Address,
    Dereference,
    Negate,
    LogNot,
    BinNot,
    FunctionCall
}


slotmap::new_key_type! { pub struct ASTNodeHandle; }

// SlotMap is just an arena allocator
pub struct AST<'a> {
    pub nodes: SlotMap<ASTNodeHandle, ASTNode<'a>>,
    pub root: Option<ASTNodeHandle>,
}

impl <'a> AST<'a> {
    pub fn new() -> AST<'a> {
        AST { nodes: SlotMap::with_key(), root: None}
    }
}

// Todo: Split into different lifetimes, one for nodes one for token references.
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode<'a> {
    // ==== Declarations: ====
    Program { 
        declarations: Vec<ASTNodeHandle>,
    },
    FunctionDecl {
        body: ASTNodeHandle,
        parameters: Vec<ASTNodeHandle>,
        identifier: Token,
        return_type: TypeInfo
    },
    ParameterDecl {
        identifier: Token,
        r#type: TypeInfo
    },
    VariableDecl {
        identifier: Token,
        initializer: Option<ASTNodeHandle>,
        r#type: TypeInfo
    },
    // ==== Expressions: ====
    IntLiteral {
        value: i32,
    },
    FunctionCall {
        symbol_ref: ASTNodeHandle,
        arguments: Vec<ASTNodeHandle>,
    },
    SymbolRef {
        identifier: Token,
        //r#type: TypeInfo<'a>,
    },
    BinaryOp {
        op: BinaryOpType,
        right: ASTNodeHandle,
        left: ASTNodeHandle,
    },
    UnaryOp {
        op: UnaryOpType,
        child: ASTNodeHandle,
        order: bool, // Wish this could be an anonymouis enum with, PREPORDEr or POSTORDEr
    },
    Ternary {
        first: ASTNodeHandle,
        second: ASTNodeHandle,
        third: ASTNodeHandle,
    },
    // ==== Statements: ====
    CompoundStmt {
        statements: Vec<ASTNodeHandle>,
        new_scope: bool,
    },
    ExpressionStmt {
        expression: ASTNodeHandle,
    },
    ReturnStmt {
        expression: ASTNodeHandle,
    },
    ForStmt {
        initializer: ASTNodeHandle,
        condition: ASTNodeHandle,
        update: ASTNodeHandle,
        body: ASTNodeHandle,
    },
    WhileStmt {
        condition: ASTNodeHandle,
        body: ASTNodeHandle,
    },
    IfStmt {
        condition: ASTNodeHandle,
        if_branch: ASTNodeHandle,
        else_branch: ASTNodeHandle,
    },
    DeclStmt {
        declarations: Vec<ASTNodeHandle>,
    },
    InlineAsm {
        assembly: &'a str,
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TraversalOrder {
    InOrder,
    PostOrder,
    PreOrder
}

pub trait Vistior<'a> {
    fn traverse(&mut self, node_h: &ASTNodeHandle) -> () {
        let order: TraversalOrder = self.get_order();

        let node: &ASTNode<'_> = &self.get_node(*node_h);

        self.entry(node);

        if order == TraversalOrder::PreOrder {
            self.operate(node);
        }

        match node {
            ASTNode::BinaryOp { op: _, right, left } => {
                self.traverse(left);
                self.traverse(right);
            }
            ASTNode::UnaryOp { op: _, child, order: _ } => {
                self.traverse(child);
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                self.traverse(symbol_ref);
                for arg in arguments.iter() {
                    self.traverse(arg);
                }
            }
            ASTNode::FunctionDecl { body, parameters, identifier: _, return_type: _ } => {
                for param in parameters.iter() {
                    self.traverse(param);
                }
                self.traverse(body);
            }
            ASTNode::VariableDecl { identifier: _, initializer, r#type: _ } => {
                if initializer.is_some() {
                    self.traverse(initializer.as_ref().unwrap()); // Why doesn't this explicitly deref??
                }
            }
            ASTNode::ReturnStmt { expression } => {
                self.traverse(expression);
            }
            ASTNode::CompoundStmt { statements, new_scope: _ } => {
                for stmt in statements.iter() {
                    self.traverse(stmt);
                }
            }
            ASTNode::IfStmt { condition, if_branch, else_branch } => {
                self.traverse(condition);
                self.traverse(if_branch);
                self.traverse(else_branch);
            }
            ASTNode::ForStmt { initializer, condition, update, body } => {
                self.traverse(initializer);
                self.traverse(condition);
                self.traverse(update);
                self.traverse(body);
            }
            ASTNode::WhileStmt { condition, body } => {
                self.traverse(condition);
                self.traverse(body);
            }
            // Terminal Nodes
            ASTNode::InlineAsm { assembly: _ } => {}
            ASTNode::ParameterDecl { identifier: _, r#type: _ } => {}
            ASTNode::IntLiteral { value: _  } => {}
            ASTNode::SymbolRef { identifier: _} => {}

            ASTNode::Program { declarations } => {
                for decl in declarations.iter() {
                    self.traverse(decl);
                }
            },
            ASTNode::Ternary { first, second, third } => {
                self.traverse(first);
                self.traverse(second);
                self.traverse(third);
            }
            ASTNode::ExpressionStmt { expression } => {
                self.traverse(expression);
            },
            ASTNode::DeclStmt { declarations } => {
                for decl in declarations.iter() {
                    self.traverse(decl);
                }
            },
            //_ => todo!()

        }

        if order == TraversalOrder::PostOrder {
            self.operate(node);
        }

        self.exit(node);
    }

    // Implementations should overload:
    fn operate(&mut self, node: &ASTNode<'a>) -> () {}

    // Implementations shuold overload
    fn entry(&mut self, node: &ASTNode<'a>) -> () {}

    fn exit(&mut self, node: &ASTNode<'a>) -> () {}

    fn get_order(&self) -> TraversalOrder;

    fn get_node(&self, node_h: ASTNodeHandle) -> ASTNode<'a>;

}

// AST Checker
pub struct ASTCheck<'a> {
    pub results: Vec<Discriminant<ASTNode<'a>>>,
    ast: &'a AST<'a>,
}

impl<'a> ASTCheck<'a>{
    pub fn new(ast: &'a AST<'a>) -> ASTCheck<'a> {
        ASTCheck { results: Vec::new(), ast}
    }
}

impl <'a> Vistior<'a> for ASTCheck<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn operate(&mut self, node: &ASTNode<'a>) -> () {
        self.results.push(std::mem::discriminant(node));
    }

    fn get_node(&self, node_h: ASTNodeHandle) -> ASTNode<'a> {
        self.ast.nodes.get(node_h).unwrap().clone()
    }
}

// AST Printer

pub struct ASTPrint<'a> {
    pub debug_mode: bool,
    pub depth: usize,
    ast: &'a AST<'a>,
}

impl <'a> ASTPrint<'a> {
    pub fn new(debug_mode: bool, ast: &'a AST<'a>) -> ASTPrint<'a> {
        ASTPrint { debug_mode , depth: 0, ast}
    }
}

impl <'a> Vistior<'a> for ASTPrint<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn operate(&mut self, node: &ASTNode<'a>) -> () {
        // TODO: Condition on debug mdoe vs pretty mode
        let whitespace_string: String = std::iter::repeat(' ').take((self.depth - 1) * 4).collect();
        if (self.debug_mode) {
            println!("{whitespace_string}{:?}", node)
        }
        else {
            println!("{whitespace_string}{}", node)
        }
        
    }

    fn entry(&mut self, node: &ASTNode<'a>) -> () {
        self.depth += 1;
    }

    fn exit(&mut self, node: &ASTNode<'a>) -> () {
        self.depth -= 1;
    }

    fn get_node(&self, node_h: ASTNodeHandle) -> ASTNode<'a> {
        self.ast.nodes.get(node_h).unwrap().clone()
    }
    
}


impl fmt::Display for ASTNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::BinaryOp { op, right, left } => {
                write!(f, "<BinaryOp, op: {:?}>", op)
            }
            ASTNode::CompoundStmt { statements, new_scope } => {
                write!(f, "<CompoundStmt>")
            }
            ASTNode::Program { declarations } => {
                write!(f, "<Program>")
            },
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                if let TokenKind::Identifier(str ) = identifier.kind.clone() {
                    write!(f, "<FunctionDecl, {}>", str)
                }
                else {
                    write!(f, "<FunctionDecl, missing name>")
                }
            },
            // TODO: Find some way to pritn type info
            ASTNode::ParameterDecl { identifier, r#type } => {
                if let TokenKind::Identifier(str ) = identifier.kind.clone() {
                    write!(f, "<ParameterDecl, {}>", str)
                }
                else {
                    write!(f, "<ParameterDecl, missing name>")
                }
            }
            ASTNode::VariableDecl { identifier, initializer, r#type } => {
                if let TokenKind::Identifier(str ) = identifier.kind.clone() {
                    write!(f, "<VariableDecl, {}>", str)
                }
                else {
                    write!(f, "<VariableDecl, missing name>")
                }
            },
            ASTNode::IntLiteral { value } => {
                write!(f, "<IntLiteral, {}>", value)
            },
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                write!(f, "<FunctionCall>")
            },
            ASTNode::SymbolRef { identifier } => {
                if let TokenKind::Identifier(str ) = identifier.kind.clone() {
                    write!(f, "<SymbolRef, {}>", str)
                }
                else {
                    write!(f, "<SymbolRef, missing name>")
                } 
            },
            ASTNode::UnaryOp { op, child, order } => {
                write!(f, "<UnaryOp, op: {:?}, preorder: {:?}>", op, order)
            },
            ASTNode::Ternary { first, second, third } => {
                write!(f, "<TernaryOp>")
            },
            ASTNode::ExpressionStmt { expression } => {
                write!(f, "<ExpressionStmt>")
            },
            ASTNode::ReturnStmt { expression } => {
                write!(f, "<ReturnStmt>")
            },
            ASTNode::ForStmt { initializer, condition, update, body } => {
                write!(f, "<ForStmt>")
            },
            ASTNode::WhileStmt { condition, body } => {
                write!(f, "<WhileStmt>")
            },
            ASTNode::IfStmt { condition, if_branch, else_branch } =>  {
                write!(f, "<IfStmt>")
            },
            ASTNode::DeclStmt { declarations } =>  {
                write!(f, "<DeclStmt>")
            },
            ASTNode::InlineAsm { assembly } => todo!(),
            
        }
        
    }
}