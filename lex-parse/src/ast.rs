use std::ops::Deref;

use crate::types::{TypeInfo};

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
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode<'a> {
    // ==== Declarations: ====
    Program { 
        declarations: Vec<Box<ASTNode<'a>>>,
    },
    FunctionDecl {
        body: Box<ASTNode<'a>>,
        parameters: Vec<Box<ASTNode<'a>>>,
        identifier: &'a str,
        return_type: TypeInfo<'a>
    },
    ParamaterDecl {
        identifier: &'a str,
        r#type: TypeInfo<'a>
    },
    VariableDecl {
        identifier: &'a str,
        initializer: Option<Box<ASTNode<'a>>>,
        r#type: TypeInfo<'a>
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
        identifier: &'a str,
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

pub trait Vistior {
    fn traverse<'a>(&mut self, node: &ASTNode<'a>) -> () {
        let order: TraversalOrder = self.get_order();

        if order == TraversalOrder::PreOrder {
            self.operate(node);
        }

        match node {
            ASTNode::BinaryOp { op, right, left } => {
                self.traverse(left);
                self.traverse(right);
            }
            ASTNode::UnaryOp { op, child, order } => {
                self.traverse(child);
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                self.traverse(symbol_ref);
                for arg in arguments.iter() {
                    self.traverse(arg);
                }
            }
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                for param in parameters.iter() {
                    self.traverse(param);
                }
                self.traverse(body);
            }
            ASTNode::VariableDecl { identifier, initializer, r#type } => {
                if initializer.is_some() {
                    self.traverse(initializer.as_ref().unwrap().as_ref()); // Why doesn't this explicitly deref??
                }
            }
            ASTNode::ReturnStmt { expression } => {
                self.traverse(expression);
            }
            ASTNode::CompoundStmt { statements, new_scope } => {
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
            ASTNode::ParamaterDecl { identifier: _, r#type: _ } => {}
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
    }

    fn operate(&mut self, _node: &ASTNode<'_>) -> () {
        todo!() // Implementations should overload
    }

    fn get_order(&self) -> TraversalOrder;


}