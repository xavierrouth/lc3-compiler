use std::cell::RefCell;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::error::{ErrorHandler, ParserError};

use crate::strings::Strings;
use crate::strings::{InternedString};

use crate::ast::{AST, ASTNodeHandle, ASTNode, UnaryOpType, BinaryOpType};

use crate::lexer::{Lexer};
use crate::token::{Token, TokenKind};
use crate::types::SpecifierInfo;
use crate::types::TypeInfo;

pub struct Parser<'a> {
    pub ast: AST,
    lexer: &'a mut Lexer<'a>,
    error_handler: Rc<RefCell<ErrorHandler>>,
    putback_stack: Vec<Token>, 
    token: Token, // The last token that was returend
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>, error_handler: Rc<RefCell<ErrorHandler>>) -> Parser<'a> {
        Parser {
            putback_stack: Vec::new(),
            token: Token::default(),
            ast: AST::new(),
            error_handler,
            lexer,
        }
    }

    pub fn parse(mut self) -> Option<AST> {
        match self.parse_translation_unit() {
            Ok(root) => {
                self.ast.root = Some(root);
                Some(self.ast)
            }
            Err(error) => {
                self.error_handler.borrow_mut().print_parser_error(error);
                None
            }
        }
    }

    fn get_token(&mut self) -> Token {
        if !self.putback_stack.is_empty() {
            self.token = self.putback_stack.pop().unwrap()
        }
        else {
            self.token = self.lexer.get_token().unwrap()
            
        }
        self.token.clone()
    }

    fn prev_token(& self) -> Token {
        return self.token.clone()
    }

    fn putback_token(& mut self, t: Token) -> () {
        self.putback_stack.push(t);
    }

    fn peek_token(& mut self) -> Token {
        let save_token = self.token.clone();
        let t = self.get_token();
        self.putback_token(t.clone());
        self.token = save_token;
        t
    }

    fn eat_token(& mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let prev = self.prev_token(); // Need to not update prev token on peek
        let t = self.get_token();
        if kind != t.kind {
            //todo
            match kind {
                TokenKind::Semicolon => {
                    let _token = self.prev_token();
                    return Err(ParserError::MissingSemicolon("Expected semicolon.".to_string(), prev))
                }
                _ => {
                    let token = self.prev_token();
                    return Err(ParserError::GeneralError("Unexpected token.".to_string(), Some(token)))
                }
            }
        }
        else {
            Ok(t)
        }
    }

    fn expect_token(& mut self, kind: TokenKind) -> bool {
        let t = self.peek_token();
        if std::mem::discriminant(&kind) == std::mem::discriminant(&t.kind) {
            true
        }
        else {
            false
        }
    }

    fn parse_translation_unit(& mut self) -> Result<ASTNodeHandle, ParserError> {
        let mut body: Vec<ASTNodeHandle> = Vec::new();
        
        while self.peek_token().kind != TokenKind::EOF {
            let decl: ASTNodeHandle = self.parse_toplevel_decl()?;
            body.push(decl);
        }

        let node = ASTNode::Program { declarations: (body) };
        Ok(self.ast.nodes.insert(node))
    }
    
    fn parse_toplevel_decl(&mut self) -> Result<ASTNodeHandle, ParserError> {

        let mut ti = self.parse_declaration_specifiers()?;

        let identifier: Option<InternedString> = self.parse_declarator(&mut ti, true);
        
        match identifier {
            None => {
                let tok = self.peek_token();
                self.lexer.process_line(tok.row);
                return Err(ParserError::MissingDeclarator(tok))
            }
            Some(string) => {
                if self.expect_token(TokenKind::OpenParen) {
                    self.parse_function_definition(ti, string)
                }
                else {
                    self.parse_declaration(&mut ti, string)
                }
            }
        }
    }

    //TODO: Error handling here
    fn parse_declaration_specifiers(&mut self) -> Result<TypeInfo, ParserError> {
        let mut ti = TypeInfo{ declarator: Vec::new(), type_specifier: SpecifierInfo::default()};
        while self.parse_declaration_specifier(& mut ti) {
            continue;
        }
        Ok(ti)
    }

    fn parse_declaration_specifier(&mut self, type_info: &mut TypeInfo) -> bool {
        match self.peek_token().kind {
            TokenKind::Int => type_info.type_specifier.marked_int = true,
            TokenKind::Void => type_info.type_specifier.marked_void = true,
            TokenKind::Char => type_info.type_specifier.marked_char = true,
            TokenKind::Static => type_info.type_specifier.marked_static = true,
            TokenKind::Const => type_info.type_specifier.marked_const = true,
            _ => {return false;}
        }
        self.get_token();
        true
    }

    // Adds a declarator to a TypeInfo
    // Add the token name to this thing
    fn parse_declarator(&mut self, _ti: &mut TypeInfo, _check_pointer: bool) -> Option<InternedString> {
        if let TokenKind::Identifier(string) = self.peek_token().kind {
            self.get_token();
            Some(string)
        }
        else {
            None
        }
    }

    fn parse_function_definition(&mut self, return_type: TypeInfo, identifier: InternedString) -> Result<ASTNodeHandle, ParserError> {    
        self.eat_token(TokenKind::OpenParen)?;

        let mut parameters: Vec<ASTNodeHandle> = Vec::new();

        while !self.expect_token(TokenKind::CloseParen) {
            let mut param_type: TypeInfo = self.parse_declaration_specifiers()?;

            // TODO: Check if there was actually a declaration specifier
            if let Some(param_identifier) = self.parse_declarator(& mut param_type, true) {
                // else ERROR
                let param = ASTNode::ParameterDecl { identifier: param_identifier, r#type: param_type };
                let handle = self.ast.nodes.insert(param);
                parameters.push(handle);
                if !self.expect_token(TokenKind::Comma) {
                    break;
                }
                else {
                    self.eat_token(TokenKind::Comma)?;
                }
            }
            else {
                return Err(ParserError::GeneralError("Expected a name for this variable.".to_string(), Some(self.prev_token())))            
            }
        }

        self.eat_token(TokenKind::CloseParen)?;

        // Check for semicolon here
        if self.expect_token(TokenKind::OpenBrace) {
            let body: ASTNodeHandle = self.parse_compound_statement()?;

            let node = ASTNode::FunctionDecl { body, parameters, identifier, return_type };
            Ok(self.ast.nodes.insert(node))
        }
        else {
            return Err(ParserError::GeneralError("Functions without bodies are not supported yet.".to_string(), Some(self.prev_token())))
        }

    }

    fn parse_declaration(&mut self, ti: &mut TypeInfo, identifier: InternedString) -> Result<ASTNodeHandle, ParserError> {
        let variable_token = self.prev_token(); // This is so busted.

        if self.expect_token(TokenKind::Equals) {
            self.eat_token(TokenKind::Equals)?;
            // Variable Declaration
            let initializer: ASTNodeHandle = self.parse_expression(0)?;

            self.eat_token(TokenKind::Semicolon)?;

            let node = ASTNode::VariableDecl { identifier: identifier, initializer: Some(initializer), r#type: ti.clone() };

            let node_h = self.ast.nodes.insert(node);
            self.error_handler.borrow_mut().tokens.insert(node_h, variable_token);
            Ok(node_h)

        }
        else {
            self.eat_token(TokenKind::Semicolon)?; 

            let node = ASTNode::VariableDecl { identifier, initializer: None, r#type: ti.clone() };
            let node_h = self.ast.nodes.insert(node);
            self.error_handler.borrow_mut().tokens.insert(node_h, variable_token);
            Ok(node_h)
        }
    }

    fn parse_compound_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::OpenBrace)?;
        let mut statements: Vec<ASTNodeHandle> = Vec::new();

        while !self.expect_token(TokenKind::EOF) && !self.expect_token(TokenKind::CloseBrace) {
            let node: ASTNodeHandle = self.parse_statement()?; // Automatically pass error up
            statements.push(node);
        }

        self.eat_token(TokenKind::CloseBrace)?;

        let node = ASTNode::CompoundStmt { statements: statements, new_scope: true };
        Ok(self.ast.nodes.insert(node))

    }

    fn parse_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        // Accept errors here?
        match self.peek_token().kind {
            // These eat semicolons for you!
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            _ => { // Thsee don't eat semicolons!
                // Attempt variable declaration
                let mut ti: TypeInfo = self.parse_declaration_specifiers()?;
                if ti.type_specifier.marked_int || ti.type_specifier.marked_char {
                    match self.parse_declarator(&mut ti, true) {
                        None => {
                            let tok = self.peek_token();
                            self.lexer.process_line(tok.row);
                            return Err(ParserError::MissingDeclarator(tok))
                        }
                        Some(identifier) => {
                            self.parse_declaration(&mut ti, identifier) // JK this does eat semicolon
                        }
                    }
                }
                else {
                    let stmt = self.parse_expression(0)?;
                    self.eat_token(TokenKind::Semicolon)?;
                    Ok(stmt)
                }
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::Return)?;
        let expression: ASTNodeHandle = self.parse_expression(0)?;

        let stmt = ASTNode::ReturnStmt { expression };
        let stmt = self.ast.nodes.insert(stmt);
        self.eat_token(TokenKind::Semicolon)?;
        Ok(stmt)
    }

    fn parse_if_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        todo!()
    }

    fn parse_while_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        todo!()
    }

    fn parse_for_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        todo!()
    }

    // Pratt Parsing
    fn parse_expression(&mut self, binding_power: u8) -> Result<ASTNodeHandle, ParserError> {
        let mut lhs: ASTNodeHandle = match self.peek_token().kind {
            TokenKind::IntLiteral(..) => self.parse_int_literal()?,
            TokenKind::Identifier(..) => self.parse_symbol_ref()?,
            TokenKind::OpenParen => {
                self.eat_token(TokenKind::OpenParen)?;
                let temp: ASTNodeHandle = self.parse_expression(0)?;
                self.eat_token(TokenKind::CloseParen)?;
                temp
            }

            token_kind @ _ => {
                // Check if this is a prefix op
                if let Some(((), r_bp) )= self.prefix_binding_power(&token_kind) {

                    // Transform tok to op
                    let op: UnaryOpType = match token_kind {
                        TokenKind::Star => UnaryOpType::Dereference, 
                        TokenKind::PlusPlus => UnaryOpType::Increment, 
                        TokenKind::And => UnaryOpType::Address, 
                        TokenKind::MinusMinus => UnaryOpType::Decrement, 
                        TokenKind::Minus => UnaryOpType::Negate, 
                        TokenKind::Exclamation => UnaryOpType::LogNot,
                        TokenKind::Tilde => UnaryOpType::BinNot,
                        TokenKind::Plus => UnaryOpType::Positive,
                        _ => {return Err(ParserError::UnknownError)}
                    };

                    self.eat_token(token_kind)?;
                    let child: ASTNodeHandle = self.parse_expression(r_bp)?;

                    let node = ASTNode::UnaryOp {op, child, order: true };
                    self.ast.nodes.insert(node)
                }
                else {
                    let token = self.prev_token();
                    return Err(ParserError::GeneralError("Expected expression.".to_string(), Some(token)))
                }
            }
        };

        // Infix and postfix expressoins:
        loop {
            if self.expect_token(TokenKind::Semicolon) || self.expect_token(TokenKind::CloseParen) {
                break;
            }

            if self.expect_token(TokenKind::Comma) {
                break;
            }
            
            // Need to make token copy, not move. This is annyoing!

            
            let token = self.peek_token();

            // Postfix Operator
            if let Some((l_bp, ())) = self.postfix_binding_power(&token.kind) {
                if l_bp < binding_power {
                    break;
                }

                if token.kind == TokenKind::OpenParen {
                    lhs = self.parse_function_call()?;
                }
                // Handle function calls somewhere
                else {
                    //self.get_token(); // What is this for?
                    let op: UnaryOpType = match token.kind {

                        TokenKind::PlusPlus => UnaryOpType::Increment, 
                        TokenKind::MinusMinus => UnaryOpType::Decrement, 
                        _ => {return Err(ParserError::UnknownError)}
                    };
                    let node = ASTNode::UnaryOp {op, child: lhs, order: false };
                    lhs = self.ast.nodes.insert(node);
                    
                }
                continue;
            }

            // Infix Operator
            if let Some((l_bp, r_bp)) = self.infix_binding_power(&token.kind) {
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
                    TokenKind::Equals => BinaryOpType::Assign,
                    _ => {return Err(ParserError::UnknownError)}
                };

                let node = ASTNode::BinaryOp { op, right: rhs, left: lhs };
                lhs = self.ast.nodes.insert(node);
                continue;

            }
            break; // Shoudln't get here
            
        };

        Ok(lhs)

    }


    fn prefix_binding_power(&self, op: &TokenKind) -> Option<((), u8)> {
        match op {
            TokenKind::Plus => Some(((), 28)),
            TokenKind::Minus => Some(((), 28)),
            TokenKind::And => Some(((), 28)),
            TokenKind::Star => Some(((), 28)),
            TokenKind::PlusPlus => Some(((), 28)),
            TokenKind::MinusMinus => Some(((), 28)),
            _ => None
        }
    }

    fn postfix_binding_power(&self, op: &TokenKind) -> Option<(u8, ())> {
        match op {
            TokenKind::OpenParen => Some((30, ())),
            TokenKind::OpenBracket => Some((30, ())),
            TokenKind::PlusPlus => Some((30, ())),
            TokenKind::MinusMinus => Some((30, ())),
            _ => None
        }
    }

    fn infix_binding_power(&self, op: &TokenKind) -> Option<(u8, u8)> {
        match op {
            TokenKind::Dot => Some((28, 29)),
            TokenKind::Arrow => Some((28, 29)),

            TokenKind::Star => Some((26, 27)),
            TokenKind::Slash => Some((26, 27)),
            TokenKind::Percent => Some((26, 27)),

            TokenKind::Plus => Some((24, 25)),
            TokenKind::Minus => Some((24, 25)),

            TokenKind::LeftShift => Some((22, 23)),
            TokenKind::RightShift => Some((22, 23)),

            TokenKind::GreaterThan => Some((14, 15)),
            TokenKind::GreaterThanEqual => Some((14, 15)),
            TokenKind::LessThan => Some((14, 15)),
            TokenKind::LessThanEqual => Some((14, 15)),

            TokenKind::EqualsEquals => Some((12, 13)),
            TokenKind::ExclamationEquals => Some((12, 13)),

            TokenKind::And => Some((10, 11)),
            TokenKind::Bar => Some((8, 9)),

            TokenKind::Equals => Some((5, 4)),

            _ => None
        }
    }

    fn parse_int_literal(&mut self) -> Result<ASTNodeHandle, ParserError> {
        let value: i32 = match self.get_token().kind {
            TokenKind::IntLiteral(value) => value,
            _ => {return Err(ParserError::UnknownError);}
        };
        let node = ASTNode::IntLiteral { value };
        Ok(self.ast.nodes.insert(node))

    }

    fn parse_symbol_ref(&mut self) -> Result<ASTNodeHandle, ParserError> {


        if let TokenKind::Identifier(identifier) = self.peek_token().kind {
            let tok = self.get_token();
            
            let node = ASTNode::SymbolRef {identifier: identifier};
            
            // TOOD: Write helper function that takes node and token and does this for you:
            let node_h = self.ast.nodes.insert(node);
            self.error_handler.borrow_mut().tokens.insert(node_h, tok);
            Ok(node_h)
        }
        else {
            return Err(ParserError::GeneralError("Expected symbol.".to_string(), Some(self.prev_token())))
        }
        
    }

    fn parse_function_call(&mut self) -> Result<ASTNodeHandle, ParserError> {
        todo!()
    }
    
}

#[cfg(test)]
mod parser_tests {

    use std::cell::RefCell;
    use std::mem::{Discriminant, discriminant};
    use std::rc::Rc;

    use crate::ast::{Vistior, ASTCheck, ASTPrint};
    use crate::lexer::{Lexer};
    use crate::parser::{Parser};
    use crate::strings::Strings;
    use super::*;
    

    #[test]
    fn var_decl() {
        let src = String::from("int a;");
        let error_handler = Rc::new(RefCell::new(ErrorHandler::new()));
        
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str(), error_handler.clone());
        
        let mut parser: Parser<'_> = Parser::new(& mut lexer, error_handler);

        let root = parser.parse_translation_unit().unwrap();
        let ast: &AST = &parser.ast;

        let mut checker: ASTCheck<'_> = ASTCheck::new(ast); 

        checker.traverse(&root);

        
        let gold: Vec<Discriminant<ASTNode>> = vec![
            discriminant(&ASTNode::Program { declarations: Vec::new()}),
            discriminant(&ASTNode::VariableDecl { identifier: Strings.lock().unwrap().get_or_intern("hi") , initializer: None, r#type: TypeInfo::default() }),
        ];

        for i in 0..checker.results.len() {
            dbg!(checker.results[i]);
            assert_eq!(checker.results[i], gold[i])
        }
         
        let mut printer: ASTPrint<'_> = ASTPrint::new(false, ast);

        printer.traverse(&root);

    }

    #[test]
    fn expression() {
        let src = String::from("5 - 10 + (3 * 11)");
        let mut error_handler = Rc::new(RefCell::new(ErrorHandler::new()));
        
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str(), error_handler.clone());
        
        let mut parser: Parser<'_> = Parser::new(& mut lexer, error_handler);

        let root = parser.parse_expression(0);
        let ast: &AST = &parser.ast;

        let mut checker: ASTCheck<'_> = ASTCheck::new(ast); 

        checker.traverse(&root.unwrap());

        /*
        let gold: Vec<Discriminant<ASTNode<'_>>> = vec![
            discriminant(&ASTNode::BinaryOp {left: Box::new(ASTNode:: {})}),
            discriminant(&ASTNode::VariableDecl() { identifier: Token::default(), initializer: None, r#type: TypeInfo::default() }),
        ];

        for i in 0..checker.results.len() {
            dbg!(checker.results[i]);
            assert_eq!(checker.results[i], gold[i])
        }
        */
    }
    
}