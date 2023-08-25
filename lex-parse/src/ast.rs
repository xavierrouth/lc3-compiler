#[derive(Debug, Clone, PartialEq)]

// Need to maintain some maps, first is debug info, which maps ASTNodes to tokens.
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd,
    LogOr,
    LogNot,
    BitAnd,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    NotEqual,
    EqualEqual,
    Assign,
}

pub enum UnaryOpType {
    Increment,
    Decrement,
    Address,
    Dereference,
}

pub enum ASTNode {
    // ==== Declarations: ====
    Program { 
        declarations: Vec<Box<ASTNode>>,
    },
    FunctionDecl {
        body: Box<ASTNode>,
        parameters: Vec<Box<ASTNode>>,
        identifier: &str,
        return_type: TypeInfo
    },
    ParamaterDecl {
        identifier: &str,
        r#type: TypeInfo
    },
    VariableDecl {
        identifier: &str,
        initializer: Box<ASTNode>,
        r#type: TypeInfo
    },
    // ==== Expressions: ====
    IntLiteral {
        value: i32,
    },
    FunctionCall {
        symbol_ref: Box<ASTNode>,
        arguments: Vec<Box<ASTNode>>,
    },
    SymbolRef {
        identifier: &str,
        r#type: TypeInfo,
    },
    BinaryOp {
        op: BinaryOpType,
        right: Box<ASTNode>,
        left: Box<ASTNode>,
    },
    UnaryOp {
        op: UnaryOpType,
        child: Box<ASTNode>,
        order: bool, // Wish this could be an anonymouis enum with, PREPORDEr or POSTORDEr
    },
    Ternary {
        first: Box<ASTNode>,
        second: Box<ASTNode>,
        third: Box<ASTNode>,
    }
    // ==== Statements: ====
    CompoundStmt {
        statements: Vec<Box<AstNode>>,
        new_scope: bool,
    },
    ExpressionStmt {
        expression: Box<ASTNode>,
    },
    ReturnStmt {
        expression: Box<ASTNode>,
    },
    ForStmt {
        initializer: Box<ASTNode>,
        condition: Box<ASTNode>,
        update: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    WhileStmt {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    IfStmt {
        condition: Box<ASTNode>,
        if_branch: Box<ASTNode>,
        else_branch: Box<ASTNode>,
    },
    DeclStmt {
        declarations: Vec<Box<ASTNode>>,
    },
    InlineAsm {
        assembly: &str,
    }
}

