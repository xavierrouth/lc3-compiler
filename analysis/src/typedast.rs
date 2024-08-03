
use lex_parse::ast::{BinaryOpType, UnaryOpType};
use slotmap::{SlotMap, SecondaryMap};
use std::fmt::{self, write, Display};
use std::mem::Discriminant;
use std::ops::Deref;

use lex_parse::types::{Type, TypePrintable};
use lex_parse::context::{InternedString, Context, InternedType};

use crate::symtab::{VarDecl};

// Need to maintain some maps, first is debug info, which maps ASTNodes to tokens.
slotmap::new_key_type! { pub struct TypedASTNodeHandle; }

// SlotMap is just an arena allocator
pub struct TypedAST {
    pub nodes: SlotMap<TypedASTNodeHandle, TypedASTNode>,
    pub root: Option<TypedASTNodeHandle>,
}

impl <'a> TypedAST {
    pub fn new() -> TypedAST {
        TypedAST { nodes: SlotMap::with_key(), root: None}
    }

    pub fn get(&self, node_h: TypedASTNodeHandle) -> &TypedASTNode {
        self.nodes.get(node_h).unwrap()
    }

    pub fn remove(&mut self, node_h: TypedASTNodeHandle) -> TypedASTNode {
        self.nodes.remove(node_h).expect("invalid call to remove")
    }

    pub fn functions(&self) -> Vec<TypedASTNodeHandle> {
        let mut vec = Vec::new();
        for (handle, data) in self.nodes.iter() {
            match data {
                TypedASTNode::FunctionDecl { .. } => vec.push(handle),
                _ => continue
            }
        }
        vec
    }
}

type VarDeclRef = VarDecl;

#[derive(Debug, Clone, PartialEq)]
pub enum TypedASTNode {
    // ==== Declarations: ====
    Program { 
        functions: Vec<TypedASTNodeHandle>,
        globals: Vec<VarDeclRef>,  // TODO: Keep these as VariableDecl typedASTNodes, instead of moving them to the program delaration during type checking.
    },
    FunctionDecl {
        body: TypedASTNodeHandle,
        identifier: InternedString,
        return_type: InternedType,

        // Local variables / Parameters.
        parameters: Vec<VarDeclRef>,
        // locals: Vec<VarDeclRef>, // TODO: Keep these as VariableDecl typedASTNodes, instead of moving them to the function delaration during type checking.
    },
    VariableDecl {
        decl: VarDeclRef,
        initializer: Option<TypedASTNodeHandle>,
        type_info: InternedType,
    },
    // Note, we have removed the Record and Field declarations, as they don't get lowered to anything. 
    // ==== Expressions: ====
    IntLiteral {
        value: i32,
    },
    // TODO: Merge these two.
    LvalueToRvalue {
        child: TypedASTNodeHandle,
        ty: InternedType,
    },
    ArrayToPointerDecay {
        child: TypedASTNodeHandle,
        ty: InternedType,
    },
    FunctionCall {
        symbol_ref: TypedASTNodeHandle,
        arguments: Vec<TypedASTNodeHandle>,
        ty: InternedType,
    },
    SymbolRef {
        identifier: InternedString,
        decl: VarDeclRef, // let this be a reference to a function.
        ty: InternedType,
    },
    FieldRef {
        identifier: InternedString,
        ty: InternedType,
    },
    BinaryOp {
        op: BinaryOpType,
        left: TypedASTNodeHandle,
        right: TypedASTNodeHandle,
        ty: InternedType,
    },
    UnaryOp {
        op: UnaryOpType,
        child: TypedASTNodeHandle,
        order: bool, // Wish this could be an anonymouis enum with, PREPORDEr or POSTORDEr
        ty: InternedType,
    },
    Ternary {
        first: TypedASTNodeHandle,
        second: TypedASTNodeHandle,
        third: TypedASTNodeHandle,
        ty: InternedType,
    },
    // ==== Statements: ====
    CompoundStmt {
        statements: Vec<TypedASTNodeHandle>,
    },
    ExpressionStmt {
        expression: TypedASTNodeHandle,
    },
    ReturnStmt {
        expression: Option<TypedASTNodeHandle>,
    },
    ForStmt {
        initializer: TypedASTNodeHandle,
        condition: TypedASTNodeHandle,
        update: TypedASTNodeHandle,
        body: TypedASTNodeHandle,
    },
    WhileStmt {
        condition: TypedASTNodeHandle,
        body: TypedASTNodeHandle,
    },
    IfStmt {
        condition: TypedASTNodeHandle,
        if_branch: TypedASTNodeHandle,
        else_branch: Option<TypedASTNodeHandle>,
    },
    BreakStmt,
    DeclStmt {
        declarations: Vec<TypedASTNodeHandle>,
    },
    
    InlineAsm {
        assembly: InternedString,
    }
}

pub struct TypedASTNodePrintable<'a> {
    pub node: TypedASTNode,
    pub context: &'a Context<'a>
}

impl Display for TypedASTNodePrintable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.node {
            TypedASTNode::BinaryOp { op, right: _, left: _, ty } => {
                write!(f, "<BinaryOp, op: {:?}>", op)
            }
            TypedASTNode::CompoundStmt { statements: _, } => {
                write!(f, "<CompoundStmt>")
            }
            TypedASTNode::Program { functions, globals} => {
                write!(f, "<Program>");
                for global in globals {
                    writeln!(f, "<var decl: {:?}>", global);
                }
                write!(f, "")
            },
            TypedASTNode::VariableDecl { decl, initializer, type_info } => {
                write!(f, "<VariableDecl, {}, {}>", self.context.resolve_string(decl.identifier), TypePrintable{data: self.context.resolve_type(type_info), context: self.context})
            }
            TypedASTNode::FunctionDecl { body: _, parameters, identifier, return_type: _ } => {
                write!(f, "<FunctionDecl, {}>", self.context.resolve_string(*identifier))
            },
            TypedASTNode::IntLiteral { value } => {
                write!(f, "<IntLiteral, {}>", value)
            },
            TypedASTNode::FunctionCall { symbol_ref: _, arguments: _, ty } => {
                write!(f, "<FunctionCall>")
            },
            TypedASTNode::SymbolRef { identifier, decl, ty } => {
                write!(f, "<SymbolRef, {}>", self.context.resolve_string(*identifier))
            },
            TypedASTNode::FieldRef { identifier, ty } => {
                write!(f, "<FieldRef, {}>", self.context.resolve_string(*identifier))
            },
            TypedASTNode::UnaryOp { op, child: _, order, ty } => {
                write!(f, "<UnaryOp, op: {:?}, preorder: {:?}>", op, order)
            },
            TypedASTNode::Ternary { first: _, second: _, third: _, ty } => {
                write!(f, "<TernaryOp>")
            },
            TypedASTNode::ExpressionStmt { expression: _ } => {
                write!(f, "<ExpressionStmt>")
            },
            TypedASTNode::ReturnStmt { expression: _ } => {
                write!(f, "<ReturnStmt>")
            },
            TypedASTNode::ForStmt { initializer: _, condition: _, update: _, body: _ } => {
                write!(f, "<ForStmt>")
            },
            TypedASTNode::WhileStmt { condition: _, body: _ } => {
                write!(f, "<WhileStmt>")
            },
            TypedASTNode::IfStmt { condition: _, if_branch: _, else_branch: _ } =>  {
                write!(f, "<IfStmt>")
            },
            TypedASTNode::DeclStmt { declarations: _ } =>  {
                write!(f, "<DeclStmt>")
            },
            TypedASTNode::InlineAsm { assembly: _ } => todo!(),
            TypedASTNode::BreakStmt => {
                write!(f, "<BreakStmt>")
            },
            TypedASTNode::LvalueToRvalue { child, ty } => {
                write!(f, "<LvalueToRvalue>")
            }
            TypedASTNode::ArrayToPointerDecay { child, ty } => todo!(),
        }
    }
}

pub trait TypedVistior<'a> {
    fn traverse(& mut self, node_h: TypedASTNodeHandle) -> () {
        if self.halt() {
            return;
        }

        let node: TypedASTNode = self.get(node_h).clone();

        self.preorder(node_h);

        match node {
            TypedASTNode::BinaryOp { op: _, right, left, ty } => {
                self.traverse(left);
                self.traverse(right);
            }
            TypedASTNode::UnaryOp { op: _, child, order: _, ty } => {
                self.traverse(child);
            }
            TypedASTNode::FunctionCall { symbol_ref, arguments, ty } => {
                self.traverse(symbol_ref);
                for &arg in arguments.iter() {
                    self.traverse(arg);
                }
            }
            TypedASTNode::FunctionDecl { body, parameters, identifier: _, return_type: _, } => {
                self.traverse(body);
            }
            /*
            TypedASTNode::VariableDecl { identifier: _, initializer, type_info: _ } => {
                if let Some(intiailizer) = initializer {
                    self.traverse(intiailizer) // Why doesn't this explicitly deref??
                };
            }  */
            TypedASTNode::ReturnStmt { expression } => {
                match expression {
                    Some(expression) => self.traverse(expression),
                    None => (),
                }
            }
            TypedASTNode::CompoundStmt { statements } => {
                for &stmt in statements.iter() {
                    self.traverse(stmt);
                }
            }
            TypedASTNode::IfStmt { condition, if_branch, else_branch } => {
                self.traverse(condition);
                self.traverse(if_branch);
                if let Some(else_branch) = else_branch {
                    self.traverse(else_branch);
                }
            }
            TypedASTNode::ForStmt { initializer, condition, update, body } => {
                self.traverse(initializer);
                self.traverse(condition);
                self.traverse(update);
                self.traverse(body);
            }
            TypedASTNode::WhileStmt { condition, body } => {
                self.traverse(condition);
                self.traverse(body);
            }
            TypedASTNode::Program { functions, globals } => {
                for &func in functions.iter() {
                    self.traverse(func);
                }
            },
            TypedASTNode::Ternary { first, second, third, ty } => {
                self.traverse(first);
                self.traverse(second);
                self.traverse(third);
            }
            TypedASTNode::ExpressionStmt { expression } => {
                self.traverse(expression);
            },
            TypedASTNode::DeclStmt { declarations } => {
                for &decl in declarations.iter() {
                    self.traverse(decl);
                }
            },
            TypedASTNode::LvalueToRvalue { child, ty } => {
                self.traverse(child);
            }
            TypedASTNode::ArrayToPointerDecay { child, ty } => {
                self.traverse(child);
            },
            TypedASTNode::VariableDecl { decl, initializer, type_info } => {
                if let Some(intiailizer) = initializer {
                    self.traverse(intiailizer)
                };
            },
            // Terminal Nodes
            TypedASTNode::InlineAsm { assembly: _ } => {}
            TypedASTNode::IntLiteral { value: _  } => {}
            TypedASTNode::SymbolRef { identifier: _, decl, ty } => {}
            TypedASTNode::FieldRef { identifier: _, ty } => {}
            TypedASTNode::BreakStmt => {}
            
        }

        self.postorder(node_h);
    }

    // Implementations must overloadd
    fn get(&self, node_h: TypedASTNodeHandle) -> &TypedASTNode;

    // Optional overload
    fn preorder(&mut self, _node_h: TypedASTNodeHandle) -> () {}

    fn postorder(&mut self, _node_h: TypedASTNodeHandle) -> () {}

    fn halt(& self) -> bool {false}

}

pub struct TypedASTPrint<'a> {
    pub debug_mode: bool,
    pub depth: usize,
    ast: &'a TypedAST,
    context: &'a Context<'a>,
}

impl <'a> TypedASTPrint<'a> {
    pub fn new(debug_mode: bool, ast: &'a TypedAST, context: &'a Context<'a>) -> TypedASTPrint<'a> {
        TypedASTPrint { context, debug_mode , depth: 0, ast}
    }
}

impl <'a> TypedVistior<'a> for TypedASTPrint<'a> {

    fn preorder(&mut self, node_h: TypedASTNodeHandle) -> () {
        self.depth += 1;
        // TODO: Condition on debug mdoe vs pretty mode
        let node = self.get(node_h);
        let whitespace_string: String = std::iter::repeat(' ').take((self.depth - 1) * 4).collect();
        if self.debug_mode {
            println!("{whitespace_string}{:?}", node)
        }
        else {
            let printable = TypedASTNodePrintable{ node: node.clone(), context: self.context};
            println!("{whitespace_string}{:}", printable);
        }
    }

    fn postorder(&mut self, _node_h: TypedASTNodeHandle) -> () {
        self.depth -= 1;
    }

    fn get(&self, node_h: TypedASTNodeHandle) -> &TypedASTNode {
        self.ast.nodes.get(node_h).unwrap()
    }
    
}