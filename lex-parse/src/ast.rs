
use slotmap::{SlotMap, SecondaryMap};
use std::fmt::{self, write, Display};
use std::mem::Discriminant;
use std::ops::Deref;

use crate::context::{InternedString, Context, InternedType};
use crate::types::{Type, TypePrintable};
use crate::token::{Token};

// Need to maintain some maps, first is debug info, which maps ASTNodes to tokens.

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    BitAnd,
    BitOr,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    NotEqual,
    EqualEqual,
    Assign,

    LeftShift,
    RightShift,
    LeftShiftAssign,
    RightShiftAssign,

    ArrayAccess,
    DotAccess,
    PointerAccess,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpType {
    Increment,
    Decrement,
    Address,
    Dereference,
    Negate,
    Positive,
    LogNot,
    BinNot,
}


#[derive(Debug, Clone, PartialEq)]
pub enum RecordType {
    Union,
    Struct,
}

impl Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordType::Union => write!(f, "union"),
            RecordType::Struct => write!(f, "struct"),
        }
    }
}

slotmap::new_key_type! { pub struct ASTNodeHandle; }



// SlotMap is just an arena allocator
pub struct AST {
    pub nodes: SlotMap<ASTNodeHandle, ASTNode>,
    pub root: Option<ASTNodeHandle>,
}

impl <'a> AST {
    pub fn new() -> AST {
        AST { nodes: SlotMap::with_key(), root: None}
    }

    pub fn get(&self, node_h: ASTNodeHandle) -> &ASTNode {
        self.nodes.get(node_h).unwrap()
    }

    // Really no way to move out of a slotmap?
    pub fn remove(&mut self, node_h: ASTNodeHandle) -> ASTNode {
        self.nodes.remove(node_h).expect("invalid call to remove")
    }

    pub fn functions(&self) -> Vec<ASTNodeHandle> {
        let mut vec = Vec::new();
        for (handle, data) in self.nodes.iter() {
            match data {
                ASTNode::FunctionDecl { .. } => vec.push(handle),
                _ => continue
            }
        }
        vec
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
        return_type: InternedType
    },
    ParameterDecl {
        identifier: InternedString,
        type_info: InternedType
    },
    VariableDecl {
        identifier: InternedString,
        initializer: Option<ASTNodeHandle>,
        type_info: InternedType
    },
    RecordDecl {
        identifier: InternedString,
        record_type: RecordType,
        fields: Vec<ASTNodeHandle>,
    },
    FieldDecl {
        identifier: InternedString,
        type_info: InternedType,
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
    },
    FieldRef {
        identifier: InternedString,
    },
    BinaryOp {
        op: BinaryOpType,
        left: ASTNodeHandle,
        right: ASTNodeHandle,
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
        expression: Option<ASTNodeHandle>,
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
        else_branch: Option<ASTNodeHandle>,
    },
    BreakStmt,
    DeclStmt {
        declarations: Vec<ASTNodeHandle>,
    },
    
    InlineAsm {
        assembly: InternedString,
    }
}

pub struct ASTNodePrintable<'a> {
    pub node: ASTNode,
    pub context: &'a Context<'a>
}

impl Display for ASTNodePrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.node {
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
                write!(f, "<FunctionDecl, {}>", self.context.resolve_string(*identifier))
            },
            // TODO: Find some way to pritn type info
            ASTNode::ParameterDecl { identifier, type_info: _ } => {
                write!(f, "<ParameterDecl, {}>", self.context.resolve_string(*identifier))
            }
            ASTNode::VariableDecl { identifier, initializer: _, type_info: type_info } => {
                write!(f, "<VariableDecl, {}, {}>", self.context.resolve_string(*identifier), TypePrintable{data: self.context.resolve_type(type_info), context: self.context}
                )
            },
            ASTNode::IntLiteral { value } => {
                write!(f, "<IntLiteral, {}>", value)
            },
            ASTNode::FunctionCall { symbol_ref: _, arguments: _ } => {
                write!(f, "<FunctionCall>")
            },
            ASTNode::SymbolRef { identifier } => {
                write!(f, "<SymbolRef, {}>", self.context.resolve_string(*identifier))
            },
            ASTNode::FieldRef { identifier } => {
                write!(f, "<FieldRef, {}>", self.context.resolve_string(*identifier))
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
            ASTNode::RecordDecl { identifier, record_type, fields } => {
                write!(f, "<RecordDecl, {record_type}, {}>", self.context.resolve_string(*identifier))
            }
            ASTNode::FieldDecl { identifier, type_info } => {
                write!(f, "<FieldDecl, {}, {}>", self.context.resolve_string(*identifier), TypePrintable{data: self.context.resolve_type(type_info), context: self.context})
            }
            ASTNode::BreakStmt => {
                write!(f, "<BreakStmt>")
            }
        }
    }
}

pub trait Vistior<'a> {

    fn traverse(& mut self, node_h: ASTNodeHandle) -> () {
        if self.halt() {
            return;
        }

        let node: ASTNode = self.get(node_h).clone();

        self.preorder(node_h);

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
                for &arg in arguments.iter() {
                    self.traverse(arg);
                }
            }
            ASTNode::FunctionDecl { body, parameters, identifier: _, return_type: _ } => {
                for &param in parameters.iter() {
                    self.traverse(param);
                }
                self.traverse(body);
            }
            ASTNode::VariableDecl { identifier: _, initializer, type_info: _ } => {
                if let Some(intiailizer) = initializer {
                    self.traverse(intiailizer) // Why doesn't this explicitly deref??
                };
            }
            ASTNode::ReturnStmt { expression } => {
                match expression {
                    Some(expression) => self.traverse(expression),
                    None => (),
                }
            }
            ASTNode::CompoundStmt { statements, new_scope: _ } => {
                for &stmt in statements.iter() {
                    self.traverse(stmt);
                }
            }
            ASTNode::IfStmt { condition, if_branch, else_branch } => {
                self.traverse(condition);
                self.traverse(if_branch);
                if let Some(else_branch) = else_branch {
                    self.traverse(else_branch);
                }
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

            ASTNode::Program { declarations } => {
                for &decl in declarations.iter() {
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
                for &decl in declarations.iter() {
                    self.traverse(decl);
                }
            },
            ASTNode::RecordDecl { identifier, record_type, fields } => {
                for &field in fields.iter() {
                    self.traverse(field);
                }
            }
            // Terminal Nodes
            ASTNode::InlineAsm { assembly: _ } => {}
            ASTNode::ParameterDecl { identifier: _, type_info: _ } => {}
            ASTNode::IntLiteral { value: _  } => {}
            ASTNode::SymbolRef { identifier: _} => {}
            ASTNode::FieldDecl { identifier: _, type_info: _ } => {}
            ASTNode::FieldRef { identifier: _} => {}
            ASTNode::BreakStmt => {}
            
        }

        self.postorder(node_h);
    }

    // Implementations must overloadd
    fn get(&self, node_h: ASTNodeHandle) -> &ASTNode;

    // Optional overload
    fn preorder(&mut self, _node_h: ASTNodeHandle) -> () {}

    fn postorder(&mut self, _node_h: ASTNodeHandle) -> () {}

    fn halt(& self) -> bool {false}

}

// AST Checker
pub struct ASTCheck<'a> {
    pub results: Vec<Discriminant<ASTNode>>,
    context: &'a Context<'a>,
    ast: &'a AST,
}

impl<'a> ASTCheck<'a>{
    pub fn new(ast: &'a AST, context: &'a Context<'a>) -> ASTCheck<'a> {
        ASTCheck { context, results: Vec::new(), ast}
    }
}

impl <'a> Vistior<'a> for ASTCheck<'a> {
    fn preorder(&mut self, node_h: ASTNodeHandle) -> () {
        let node = self.get(node_h);
        self.results.push(std::mem::discriminant(&node));
    }

    // Why is this clone?
    fn get(&self, node_h: ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(node_h).unwrap()
    }
}

// AST Printer

pub struct ASTPrint<'a> {
    pub debug_mode: bool,
    pub depth: usize,
    ast: &'a AST,
    context: &'a Context<'a>,
}

impl <'a> ASTPrint<'a> {
    pub fn new(debug_mode: bool, ast: &'a AST, context: &'a Context<'a>) -> ASTPrint<'a> {
        ASTPrint { context, debug_mode , depth: 0, ast}
    }
}

impl <'a> Vistior<'a> for ASTPrint<'a> {

    fn preorder(&mut self, node_h: ASTNodeHandle) -> () {
        self.depth += 1;
        // TODO: Condition on debug mdoe vs pretty mode
        let node = self.get(node_h);
        let whitespace_string: String = std::iter::repeat(' ').take((self.depth - 1) * 4).collect();
        if self.debug_mode {
            println!("{whitespace_string}{:?}", node)
        }
        else {
            let printable = ASTNodePrintable{ node: node.clone(), context: self.context};
            println!("{whitespace_string}{:}", printable);
        }
    }

    fn postorder(&mut self, _node_h: ASTNodeHandle) -> () {
        self.depth -= 1;
    }

    fn get(&self, node_h: ASTNodeHandle) -> &ASTNode {
        self.ast.nodes.get(node_h).unwrap()
    }
    
}
