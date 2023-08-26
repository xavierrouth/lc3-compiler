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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

    fn parse_translation_unit(& mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        let mut body: Vec<Box<ASTNode<'a>>> = Vec::new();
        
        while self.peek_token().kind != TokenKind::EOF {
            let node: Box<ASTNode<'a>> = self.parse_toplevel_decl()?;
            body.push(node);
        }

        Err(ParserError::UnknownError)

        //Ok(Box::new(ASTNode::Program { declarations: (body) }))
    }
    
    fn parse_toplevel_decl(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
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
        todo!()
    }

    fn parse_function_definition(&mut self, mut ti: &mut TypeInfo) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }

    fn parse_declaration(&mut self, ti: &mut TypeInfo) -> Result<Box<ASTNode<'a>>, ParserError> {

        if self.expect_token(TokenKind::Equals) {
            self.eat_token(TokenKind::Equals)?;
            // Variable Declaration
            let initializer: Box<ASTNode<'a>> = self.parse_expression(0)?;

            self.eat_token(TokenKind::Semicolon)?;

            Ok(Box::new(ASTNode::VariableDecl 
                { identifier: ti.identifier, initializer: Some(initializer), r#type: ti.clone() })
            )
        }
        else {
            self.eat_token(TokenKind::Semicolon)?;
            Ok(Box::new(ASTNode::VariableDecl { identifier: ti.identifier.unwrap().as_str(), initializer: None, r#type: ti.clone() }))
        }
    }

    fn parse_compound_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        self.eat_token(TokenKind::OpenBrace)?;
        let mut statements: Vec<Box<ASTNode<'a>>> = Vec::new();

        while !self.expect_token(TokenKind::EOF) && !self.expect_token(TokenKind::CloseBrace) {
            let node: Box<ASTNode<'a>> = self.parse_statement()?; // Automatically pass error up
            statements.push(node);
        }

        self.eat_token(TokenKind::CloseBrace)?;
        Ok(Box::new(ASTNode::CompoundStmt { statements: statements, new_scope: true }))
    }

    fn parse_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        // Accept errors here?
        match self.peek_token().kind {
            // These eat semicolons for you!
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            _ => { // Thsee don't eat semicolons!
                // Attempt variable declaration
                let mut ti = self.parse_declaration_specifiers()?;
                if ti.type_specifier.marked_int || ti.type_specifier.marked_char {
                    self.parse_declarator(&mut ti);
                    self.parse_declaration(&mut ti) // JK this does eat semicolon
                }
                else {
                    let stmt: Result<Box<ASTNode<'a>>, ParserError> = self.parse_expression(0);
                    self.eat_token(TokenKind::Semicolon)?;
                    stmt
                }
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        self.eat_token(TokenKind::Return)?;
        let expression: Box<ASTNode<'a>> = self.parse_expression(0)?;
        let stmt: Result<Box<ASTNode<'a>>, ParserError> = Ok(Box::new(ASTNode::ReturnStmt { expression }));
        self.eat_token(TokenKind::Semicolon)?;
        stmt
    }

    fn parse_if_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }

    fn parse_while_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }

    fn parse_for_statement(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }

    // Pratt Parsing
    fn parse_expression(&mut self, binding_power: i32) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }
    
}
