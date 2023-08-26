

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

// Todo: Split into different lifetimes, one for nodes one for token references.
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode<'a> {
    // ==== Declarations: ====
    Program { 
        declarations: Vec<Box<ASTNode<'a>>>,
    },
    FunctionDecl {
        body: Box<ASTNode<'a>>,
        parameters: Vec<Box<ASTNode<'a>>>,
        identifier: Token,
        return_type: TypeInfo
    },
    ParameterDecl {
        identifier: Token,
        r#type: TypeInfo
    },
    VariableDecl {
        identifier: Token,
        initializer: Option<Box<ASTNode<'a>>>,
        r#type: TypeInfo
    },
    // ==== Expressions: ====
    IntLiteral {
        value: i32,
    },
    FunctionCall {
        symbol_ref: Box<ASTNode<'a>>,
        arguments: Vec<Box<ASTNode<'a>>>,
    },
    SymbolRef {
        identifier: Token,
        //r#type: TypeInfo<'a>,
    },
    BinaryOp {
        op: BinaryOpType,
        right: Box<ASTNode<'a>>,
        left: Box<ASTNode<'a>>,
    },
    UnaryOp {
        op: UnaryOpType,
        child: Box<ASTNode<'a>>,
        order: bool, // Wish this could be an anonymouis enum with, PREPORDEr or POSTORDEr
    },
    Ternary {
        first: Box<ASTNode<'a>>,
        second: Box<ASTNode<'a>>,
        third: Box<ASTNode<'a>>,
    },
    // ==== Statements: ====
    CompoundStmt {
        statements: Vec<Box<ASTNode<'a>>>,
        new_scope: bool,
    },
    ExpressionStmt {
        expression: Box<ASTNode<'a>>,
    },
    ReturnStmt {
        expression: Box<ASTNode<'a>>,
    },
    ForStmt {
        initializer: Box<ASTNode<'a>>,
        condition: Box<ASTNode<'a>>,
        update: Box<ASTNode<'a>>,
        body: Box<ASTNode<'a>>,
    },
    WhileStmt {
        condition: Box<ASTNode<'a>>,
        body: Box<ASTNode<'a>>,
    },
    IfStmt {
        condition: Box<ASTNode<'a>>,
        if_branch: Box<ASTNode<'a>>,
        else_branch: Box<ASTNode<'a>>,
    },
    DeclStmt {
        declarations: Vec<Box<ASTNode<'a>>>,
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
    fn traverse(&mut self, node: &ASTNode<'a>) -> () {
        let order: TraversalOrder = self.get_order();

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
                    self.traverse(initializer.as_ref().unwrap().as_ref()); // Why doesn't this explicitly deref??
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


}

// AST Checker
pub struct ASTCheck<'a> {
    pub results: Vec<Discriminant<ASTNode<'a>>>,
}

impl<'a> ASTCheck<'a>{
    pub fn new() -> ASTCheck<'a> {
        ASTCheck { results: Vec::new() }
    }
}

impl <'a> Vistior<'a> for ASTCheck<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn operate(&mut self, node: &ASTNode<'a>) -> () {
        self.results.push(std::mem::discriminant(node));
    }
}

// AST Printer

pub struct ASTPrint {
    pub debug_mode: bool,
    pub depth: usize,
}

impl <'a> ASTPrint {
    pub fn new(debug_mode: bool) -> ASTPrint {
        ASTPrint { debug_mode , depth: 0}
    }
}

impl <'a> Vistior<'a> for ASTPrint {
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