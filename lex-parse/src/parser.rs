use std::error;
use std::fmt;

use crate::ast::ASTNode;
use crate::lexer::{Lexer};
use crate::token::{Token, TokenKind};
use crate::types::DeclaratorPart;
use crate::types::TypeInfo;

#[derive(Debug)]
pub enum ParserError {
    FloatError(String),
    UnknownError
}

impl error::Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::FloatError(msg) => write!(f, "{}", msg),
            ParserError::UnknownError => write!(f, "Something went wrong"),
        }
    }
}

pub struct Parser<'a> {
    putback_stack: Vec<Token>, 
    lexer: &'a mut Lexer<'a>,
    previous_token: Token, // Why do we need this again when we can just do putback?
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    fn get_token(&mut self) -> Token {
        if !self.putback_stack.is_empty() {
            self.putback_stack.pop().unwrap()
        }
        else {
            self.lexer.get_token().unwrap()
        }
    }

    fn putback_token(& mut self, t: Token) -> () {
        self.putback_stack.push(t);
    }

    fn peek_token(& mut self) -> Token {
        let t = self.get_token();
        self.putback_token(t.clone());
        t
    }

    fn eat_token(& mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let t = self.get_token();
        if kind != t.kind {
            //todo
            match kind {
                _ => Err(ParserError::UnknownError)
            }
        }
        else {
            Ok(t)
        }
    }

    fn expect_token(& mut self, kind: TokenKind) -> bool {
        let t = self.peek_token();
        if kind == t.kind {
            true
        }
        else {
            false
        }
    }

    fn parse_translation_unit(& mut self) -> Result<Box<ASTNode>, ParserError> {
        let mut body: Vec<Box<ASTNode>> = Vec::new();

        while self.peek_token().kind != TokenKind::EOF {
            match self.parse_toplevel_decl() {
                Err(E) => {return Err(E);}, // Pass Error Up
                Ok(Node) => {
                    body.push(Node);
                }
            }
        }

        Ok(Box::new(ASTNode::Program { declarations: (body) }))

    }
    
    fn parse_toplevel_decl(&mut self) -> Result<Box<ASTNode>, ParserError> {
        let t = self.peek_token();
        let mut ti: TypeInfo = self.parse_declaration_specifiers().unwrap();

        self.parse_declarator(&mut ti);

        if ti.identifier.is_none() {
            // Error
            panic!()
        }

        if self.expect_token(TokenKind::OpenParen) {
            self.parse_function_definition(&mut ti)
        }
        else {
            self.parse_declaration(&mut ti)
        }
    }

    fn parse_declaration_specifiers(&mut self) -> Result<TypeInfo, ParserError> {
        Err(ParserError::UnknownError)
    }

    // Adds a declarator to a TypeInfo
    fn parse_declarator(&mut self, mut ti: &mut TypeInfo) -> () {

    }

    fn parse_function_definition(&mut self, mut ti: &mut TypeInfo) -> Result<Box<ASTNode>, ParserError> {

    }

    fn parse_declaration(&mut self, mut ti: &mut TypeInfo) -> Result<Box<ASTNode>, ParserError> {
        if self.expect_token(TokenKind::Equals) {
            self.eat_token(TokenKind::Equals);
            // Variable Declaration
            match self.parse_expression(0) {
                Err(E) => {return Err(E);}, 
                Ok(Node) => {
                    let mut initializer = Node;
                    self.eat_token(TokenKind::Semicolon);
                    Ok(Box::new(ASTNode::VariableDecl { identifier: ti.identifier.unwrap().as_str(), initializer: initializer, r#type: ti.clone() }))
                }
            }
        }
        else {
            self.eat_token(TokenKind::Semicolon);
            Ok(Box::new(ASTNode::VariableDecl { identifier: ti.identifier.unwrap().as_str(), initializer: None, r#type: ti }))
        }
    }

    fn parse_compound_statement(&mut self) -> Result<Box<ASTNode>, ParserError> {
        self.eat_token(TokenKind::OpenBrace);
        let mut statements: Vec<Box<ASTNode>> = Vec::new();
        while !self.expect_token(TokenKind::EOF) && !self.expect_token(TokenKind::CloseBrace) {
            match self.parse_statement() {
                Err(E) => {return Err(E);}, // Pass Error Up
                Ok(Node) => {
                    statements.push(Node);
                }
            }
        }
        self.eat_token(TokenKind::CloseBrace);
        Ok(Box::new(ASTNode::CompoundStmt { statements: statements, new_scope: true }))
    }

    fn parse_statement(&mut self) -> Result<Box<ASTNode>, ParserError> {

    }
    
    // Pratt Parsing
    fn parse_expression(&mut self, binding_power: i32) -> Result<Box<ASTNode>, ParserError> {

    }
    
}
