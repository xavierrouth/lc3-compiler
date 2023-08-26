use std::error;
use std::fmt;

use crate::ast::{ASTNode, UnaryOpType, BinaryOpType};
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
    putback_stack: Vec<Token<'a>>, 
    lexer: &'a mut Lexer<'a>,
    previous_token: Token<'a>, // Why do we need this again when we can just do putback?
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    fn get_token(&mut self) -> Token<'a> {
        if !self.putback_stack.is_empty() {
            self.putback_stack.pop().unwrap()
        }
        else {
            self.lexer.get_token().unwrap()
        }
    }

    fn putback_token(& mut self, t: Token<'a>) -> () {
        self.putback_stack.push(t);
    }

    fn peek_token(& mut self) -> Token<'a> {
        let t = self.get_token();
        self.putback_token(t.clone());
        t
    }

    fn eat_token(& mut self, kind: TokenKind<'a>) -> Result<Token<'a>, ParserError> {
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

    fn expect_token(& mut self, kind: TokenKind<'a>) -> bool {
        let t = self.peek_token();
        if std::mem::discriminant(&kind) == std::mem::discriminant(&t.kind) {
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

        Ok(Box::new(ASTNode::Program { declarations: (body) }))
    }
    
    fn parse_toplevel_decl(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        let t = self.peek_token();
        let mut ti: TypeInfo<'a> = self.parse_declaration_specifiers().unwrap();

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

    fn parse_declaration_specifiers(&mut self) -> Result<TypeInfo<'a>, ParserError> {
        Err(ParserError::UnknownError)
    }

    // Adds a declarator to a TypeInfo
    fn parse_declarator(&mut self, mut ti: &mut TypeInfo<'a>) -> () {
        todo!()
    }

    fn parse_function_definition(&mut self, mut ti: &mut TypeInfo<'a>) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }

    fn parse_declaration(&mut self, ti: &mut TypeInfo<'a>) -> Result<Box<ASTNode<'a>>, ParserError> {

        if self.expect_token(TokenKind::Equals) {
            self.eat_token(TokenKind::Equals)?;
            // Variable Declaration
            let initializer: Box<ASTNode<'a>> = self.parse_expression(0)?;

            self.eat_token(TokenKind::Semicolon)?;

            Ok(Box::new(ASTNode::VariableDecl 
                { identifier: ti.identifier.unwrap(), initializer: Some(initializer), r#type: ti.clone() })
            )
        }
        else {
            self.eat_token(TokenKind::Semicolon)?; 
            Ok(Box::new(ASTNode::VariableDecl { identifier: ti.identifier.unwrap(), initializer: None, r#type: ti.clone() }))
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
                let mut ti: TypeInfo<'a> = self.parse_declaration_specifiers()?;
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
    fn parse_expression(&mut self, binding_power: u8) -> Result<Box<ASTNode<'a>>, ParserError> {
        let mut lhs: Box<ASTNode<'a>> = match self.peek_token().kind {
            TokenKind::IntLiteral(..) => self.parse_int_literal()?,
            TokenKind::Identifier(..) => self.parse_symbol_ref()?,
            TokenKind::OpenParen => {
                self.eat_token(TokenKind::OpenParen)?;
                let temp: Box<ASTNode<'a>> = self.parse_expression(0)?;
                self.eat_token(TokenKind::CloseParen)?;
                temp
            }

            tok @ _ => {
                // Check if this is a prefix op
                let ((), r_bp) = self.prefix_binding_power(tok.clone())?;
                let child: Box<ASTNode<'a>> = self.parse_expression(r_bp)?;

                // Transform tok to op
                let op: UnaryOpType = match tok {
                    TokenKind::Star => UnaryOpType::Dereference, 
                    TokenKind::PlusPlus => UnaryOpType::Increment, 
                    TokenKind::And => UnaryOpType::Address, 
                    TokenKind::MinusMinus => UnaryOpType::Decrement, 
                    TokenKind::Minus => UnaryOpType::Negate, 
                    TokenKind::Exclamation => UnaryOpType::LogNot,
                    TokenKind::Tilde => UnaryOpType::BinNot,
                    _ => {return Err(ParserError::UnknownError)}
                };
                Box::new(ASTNode::UnaryOp {op, child, order: true })
            }
        };

        loop {
            if self.expect_token(TokenKind::Semicolon) || self.expect_token(TokenKind::CloseParen) {
                break;
            }

            if self.expect_token(TokenKind::Comma) {
                break;
            }
            
            // Need to make token copy, not move. This is annyoing!

            // If its postfix op
            let token = self.peek_token();
            if let Some((l_bp, ())) = self.postfix_binding_power(token.kind.clone()) {
                if l_bp < binding_power {
                    break;
                }

                self.eat_token(token.kind.clone())?;
                
                if token.kind == TokenKind::OpenParen {
                    lhs = self.parse_function_call()?;
                }
                // Handle function calls somewhere
                else {
                    let op: UnaryOpType = match token.kind {

                        TokenKind::PlusPlus => UnaryOpType::Increment, 
                        TokenKind::MinusMinus => UnaryOpType::Decrement, 
                        _ => {return Err(ParserError::UnknownError)}
                    };
                    lhs = Box::new(ASTNode::UnaryOp {op, child: lhs, order: false });
                }

                continue;
            }

            let (l_bp, r_bp) = self.infix_binding_power(token.kind.clone())?;

            if l_bp < binding_power {
                break;
            }

            self.get_token();

            // TODO: Test for ternay '?' then ':'

            let rhs = self.parse_expression(r_bp)?;

            let op: BinaryOpType = match token.kind {
                TokenKind::Plus => BinaryOpType::Add, 
                TokenKind::Minus => BinaryOpType::Sub,
                TokenKind::Star => BinaryOpType::Mul,  
                TokenKind::Slash => BinaryOpType::Div,
                TokenKind::Percent => BinaryOpType::Mod, 
                TokenKind::AndAnd => BinaryOpType::LogAnd,
                TokenKind::BarBar => BinaryOpType::LogOr,
                _ => {return Err(ParserError::UnknownError)}
            };

            lhs = Box::new(ASTNode::BinaryOp { op, right: rhs, left: lhs });
        };

        Ok(lhs)

    }


    fn prefix_binding_power(&self, op: TokenKind<'a>) -> Result<((), u8), ParserError> {
        match op {
            TokenKind::Plus => Ok(((), 28)),
            TokenKind::Minus => Ok(((), 28)),
            TokenKind::And => Ok(((), 28)),
            TokenKind::Star => Ok(((), 28)),
            TokenKind::PlusPlus => Ok(((), 28)),
            TokenKind::MinusMinus => Ok(((), 28)),
            _ => Err(ParserError::UnknownError)
        }
    }

    fn postfix_binding_power(&self, op: TokenKind<'a>) -> Option<(u8, ())> {
        match op {
            TokenKind::OpenParen => Some((30, ())),
            TokenKind::OpenBracket => Some((30, ())),
            TokenKind::PlusPlus => Some((30, ())),
            TokenKind::MinusMinus => Some((30, ())),
            _ => None
        }
    }

    fn infix_binding_power(&self, op: TokenKind<'a>) -> Result<(u8, u8), ParserError> {
        match op {
            TokenKind::Dot => Ok((28, 29)),
            TokenKind::Arrow => Ok((28, 29)),

            TokenKind::Star => Ok((26, 27)),
            TokenKind::Slash => Ok((26, 27)),
            TokenKind::Percent => Ok((26, 27)),

            TokenKind::Plus => Ok((24, 25)),
            TokenKind::Minus => Ok((24, 25)),

            TokenKind::LeftShift => Ok((22, 23)),
            TokenKind::RightShift => Ok((22, 23)),

            TokenKind::GreaterThan => Ok((14, 15)),
            TokenKind::GreaterThanEqual => Ok((14, 15)),
            TokenKind::LessThan => Ok((14, 15)),
            TokenKind::LessThanEqual => Ok((14, 15)),

            TokenKind::EqualsEquals => Ok((12, 13)),
            TokenKind::ExclamationEquals => Ok((12, 13)),

            TokenKind::And => Ok((10, 11)),
            TokenKind::Bar => Ok((8, 9)),

            TokenKind::Equals => Ok((5, 4)),

            _ => Err(ParserError::UnknownError)
        }
    }

    fn parse_int_literal(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        let value: i32 = match self.get_token().kind {
            TokenKind::IntLiteral(value) => value,
            _ => {return Err(ParserError::UnknownError);}
        };
        Ok(Box::new(ASTNode::IntLiteral { value }))
    }

    fn parse_symbol_ref(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        let value: String = match self.get_token().kind {
            TokenKind::Identifier(value) => value,
            _ => {return Err(ParserError::UnknownError);}
        };
        Ok(Box::new(ASTNode::SymbolRef { identifier: (value.as_str()) }))
    }

    fn parse_function_call(&mut self) -> Result<Box<ASTNode<'a>>, ParserError> {
        todo!()
    }
    
}
