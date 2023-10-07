
use lex_parse::ast::{BinaryOpType, UnaryOpType};
use slotmap::{SlotMap, SecondaryMap};
use std::fmt::{self, write, Display};
use std::mem::Discriminant;
use std::ops::Deref;

use lex_parse::types::{Type, TypePrintable};
use lex_parse::context::{InternedString, Context, InternedType};

use crate::symtab::VarDecl;

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
    Empty,
    Program { 
        functions: Vec<TypedASTNodeHandle>,
        globals: Vec<VarDeclRef>, // Data section.
    },
    FunctionDecl {
        body: TypedASTNodeHandle,
        identifier: InternedString,
        return_type: InternedType,

        // Local variables / Parameters.
        parameters: Vec<VarDeclRef>,
        locals: Vec<VarDeclRef>,
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
            _ => todo!()
        }
    }
}
