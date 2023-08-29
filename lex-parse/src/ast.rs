
use slotmap::{SlotMap, SecondaryMap};
use std::fmt;
use std::mem::Discriminant;

use crate::strings::{InternedString, Strings};
use crate::types::{TypeInfo};
use crate::token::{Token};

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
pub struct AST {
    pub nodes: SlotMap<ASTNodeHandle, ASTNode>,
    pub tokens: SecondaryMap<ASTNodeHandle, Token>, 
    pub root: Option<ASTNodeHandle>,
}

impl <'a> AST {
    pub fn new() -> AST {
        AST { nodes: SlotMap::with_key(), tokens: SecondaryMap::new(), root: None}
    }
}

// Todo: Split into different lifetimes, one for nodes one for token references.
#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    // ==== Declarations: ====
    Program { 
        declarations: Vec<ASTNodeHandle>,
    },
    FunctionDecl {
        body: ASTNodeHandle,
        parameters: Vec<ASTNodeHandle>,
        identifier: InternedString,
        return_type: TypeInfo
    },
    ParameterDecl {
        identifier: InternedString,
        r#type: TypeInfo
    },
    VariableDecl {
        identifier: InternedString,
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
        identifier: InternedString,
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
        assembly: InternedString,
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

        let node: ASTNode = self.get_node(node_h).clone();

        self.entry(node_h);

        if order == TraversalOrder::PreOrder {
            self.operate(node_h);
        }

        match node {
            ASTNode::BinaryOp { op: _, right, left } => {
                self.traverse(&left);
                self.traverse(&right);
            }
            ASTNode::UnaryOp { op: _, child, order: _ } => {
                self.traverse(&child);
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => {
                self.traverse(&symbol_ref);
                for arg in arguments.iter() {
                    self.traverse(arg);
                }
            }
            ASTNode::FunctionDecl { body, parameters, identifier: _, return_type: _ } => {
                for param in parameters.iter() {
                    self.traverse(param);
                }
                self.traverse(&body);
            }
            ASTNode::VariableDecl { identifier: _, initializer, r#type: _ } => {
                if initializer.is_some() {
                    self.traverse(initializer.as_ref().unwrap()); // Why doesn't this explicitly deref??
                }
            }
            ASTNode::ReturnStmt { expression } => {
                self.traverse(&expression);
            }
            ASTNode::CompoundStmt { statements, new_scope: _ } => {
                for stmt in statements.iter() {
                    self.traverse(stmt);
                }
            }
            ASTNode::IfStmt { condition, if_branch, else_branch } => {
                self.traverse(&condition);
                self.traverse(&if_branch);
                self.traverse(&else_branch);
            }
            ASTNode::ForStmt { initializer, condition, update, body } => {
                self.traverse(&initializer);
                self.traverse(&condition);
                self.traverse(&update);
                self.traverse(&body);
            }
            ASTNode::WhileStmt { condition, body } => {
                self.traverse(&condition);
                self.traverse(&body);
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
                self.traverse(&first);
                self.traverse(&second);
                self.traverse(&third);
            }
            ASTNode::ExpressionStmt { expression } => {
                self.traverse(&expression);
            },
            ASTNode::DeclStmt { declarations } => {
                for decl in declarations.iter() {
                    self.traverse(decl);
                }
            },
            //_ => todo!()

        }

        if order == TraversalOrder::PostOrder {
            self.operate(node_h);
        }

        self.exit(node_h);
    }

    // Implementations should overload:
    fn operate(&mut self, _node_h: &ASTNodeHandle) -> () {}

    // Implementations shuold overload
    fn entry(&mut self, _node_h: &ASTNodeHandle) -> () {}

    fn exit(&mut self, _node_h: &ASTNodeHandle) -> () {}

    fn get_order(&self) -> TraversalOrder;

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode;

}

// AST Checker
pub struct ASTCheck<'a> {
    pub results: Vec<Discriminant<ASTNode>>,
    ast: &'a AST,
}

impl<'a> ASTCheck<'a>{
    pub fn new(ast: &'a AST) -> ASTCheck<'a> {
        ASTCheck { results: Vec::new(), ast}
    }
}

impl <'a> Vistior<'a> for ASTCheck<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn operate(&mut self, node_h: &ASTNodeHandle) -> () {
        let node = self.get_node(node_h);
        self.results.push(std::mem::discriminant(&node));
    }

    // Why is this clone?
    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }
}

// AST Printer

pub struct ASTPrint<'a> {
    pub debug_mode: bool,
    pub depth: usize,
    ast: &'a AST,
}

impl <'a> ASTPrint<'a> {
    pub fn new(debug_mode: bool, ast: &'a AST) -> ASTPrint<'a> {
        ASTPrint { debug_mode , depth: 0, ast}
    }
}

impl <'a> Vistior<'a> for ASTPrint<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn operate(&mut self, node_h: &ASTNodeHandle) -> () {
        // TODO: Condition on debug mdoe vs pretty mode
        let node = self.get_node(node_h);
        let whitespace_string: String = std::iter::repeat(' ').take((self.depth - 1) * 4).collect();
        if self.debug_mode {
            println!("{whitespace_string}{:?}", node)
        }
        else {
            println!("{whitespace_string}{}", node)
        }
        
    }

    fn entry(&mut self, _node_h: &ASTNodeHandle) -> () {
        self.depth += 1;
    }

    fn exit(&mut self, _node_h: &ASTNodeHandle) -> () {
        self.depth -= 1;
    }

    fn get_node(&self, node_h: &ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(*node_h).unwrap()
    }
    
}


impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::BinaryOp { op, right: _, left: _ } => {
                write!(f, "<BinaryOp, op: {:?}>", op)
            }
            ASTNode::CompoundStmt { statements: _, new_scope: _ } => {
                write!(f, "<CompoundStmt>")
            }
            ASTNode::Program { declarations: _ } => {
                write!(f, "<Program>")
            },
            ASTNode::FunctionDecl { body: _, parameters: _, identifier, return_type: _ } => {
                
                write!(f, "<FunctionDecl, {}>", Strings.lock().unwrap().resolve(*identifier).unwrap())
            },
            // TODO: Find some way to pritn type info
            ASTNode::ParameterDecl { identifier, r#type: _ } => {
                write!(f, "<ParameterDecl, {}>", Strings.lock().unwrap().resolve(*identifier).unwrap())
            }
            ASTNode::VariableDecl { identifier, initializer: _, r#type: _ } => {
                write!(f, "<VariableDecl, {}>", Strings.lock().unwrap().resolve(*identifier).unwrap())
            },
            ASTNode::IntLiteral { value } => {
                write!(f, "<IntLiteral, {}>", value)
            },
            ASTNode::FunctionCall { symbol_ref: _, arguments: _ } => {
                write!(f, "<FunctionCall>")
            },
            ASTNode::SymbolRef { identifier } => {
                write!(f, "<SymbolRef, {}>", Strings.lock().unwrap().resolve(*identifier).unwrap())
            },
            ASTNode::UnaryOp { op, child: _, order } => {
                write!(f, "<UnaryOp, op: {:?}, preorder: {:?}>", op, order)
            },
            ASTNode::Ternary { first: _, second: _, third: _ } => {
                write!(f, "<TernaryOp>")
            },
            ASTNode::ExpressionStmt { expression: _ } => {
                write!(f, "<ExpressionStmt>")
            },
            ASTNode::ReturnStmt { expression: _ } => {
                write!(f, "<ReturnStmt>")
            },
            ASTNode::ForStmt { initializer: _, condition: _, update: _, body: _ } => {
                write!(f, "<ForStmt>")
            },
            ASTNode::WhileStmt { condition: _, body: _ } => {
                write!(f, "<WhileStmt>")
            },
            ASTNode::IfStmt { condition: _, if_branch: _, else_branch: _ } =>  {
                write!(f, "<IfStmt>")
            },
            ASTNode::DeclStmt { declarations: _ } =>  {
                write!(f, "<DeclStmt>")
            },
            ASTNode::InlineAsm { assembly: _ } => todo!(),
            
        }
        
    }
}