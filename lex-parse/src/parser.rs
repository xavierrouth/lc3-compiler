use std::cell::RefCell;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::error::{ErrorHandler, ParserError};

use crate::context::{InternedString, InternedType, TypeInterner, Context};

use crate::ast::{AST, ASTNodeHandle, ASTNode, UnaryOpType, BinaryOpType};

use crate::lexer::{Lexer};
use crate::token::{Token, TokenKind};

use crate::types::CType;
use crate::types::CVQual;
use crate::types::StorageQual;
use crate::types::{TypeSpecifier, DeclaratorPart, Type};

pub struct Parser<'a> {
    pub ast: AST,
    lexer: &'a mut Lexer<'a, 'a>,
    putback_stack: Vec<Token>, 
    token: Token, // The last token that was returend

    context: &'a Context<'a>, //TODO: Merge error handler and context.
    error_handler: &'a ErrorHandler<'a>,
}
impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a, 'a>, context: &'a Context<'a>, error_handler: &'a ErrorHandler<'a>,) -> Parser<'a> {
        Parser {
            putback_stack: Vec::new(),
            token: Token::default(),
            ast: AST::new(),
            lexer,
            context,
            error_handler,
        }
    }

    pub fn parse(mut self) -> Result<AST, ParserError> {
        match self.parse_translation_unit() {
            Ok(root) => {
                self.ast.root = Some(root);
                Ok(self.ast)
            }
            Err(error) => Err(error)
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
        let prev: Token = self.prev_token(); // Need to not update prev token on peek
        let t: Token = self.get_token();
        if kind != t.kind {
            //todo
            match kind {
                TokenKind::Semicolon => {
                    return Err(ParserError::MissingSemicolon(prev))
                }
                TokenKind::Comma => {
                    return Err(ParserError::GeneralError("Unexpected token.".to_string(), Some(t)))
                }
                _ => {
                    return Err(ParserError::GeneralError("Unexpected token.".to_string(), Some(t)))
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

        let specifier = self.parse_declaration_specifiers()?;

        let (declarator, identifier) = self.parse_declarator(false, true)?;
        
        match identifier {
            None => {
                let tok = self.peek_token();
                return Err(ParserError::MissingDeclarator(tok))
            }
            Some(string) => {
                let type_info = self.context.get_type(&Type {
                    declarator,
                    specifier,
                });
                if self.expect_token(TokenKind::OpenParen) {
                    self.parse_function_definition(type_info, string)
                }
                else {
                    self.parse_declaration(type_info, string)
                }
            }
        }
    }

    //TODO: Error handling here
    fn parse_declaration_specifiers(&mut self) -> Result<TypeSpecifier, ParserError> {
        let mut specifier = TypeSpecifier::default();
        while self.parse_declaration_specifier(& mut specifier)? {
            continue;
        }
        Ok(specifier)
    }

    fn parse_declaration_specifier(&mut self, specifier: &mut TypeSpecifier) -> Result<bool, ParserError> {

        // TOOD: Check to see if these aren't set already.
        match self.peek_token().kind {
            TokenKind::Int => specifier.ctype = Some(CType::Int),
            TokenKind::Void => specifier.ctype = Some(CType::Void),
            TokenKind::Char => specifier.ctype = Some(CType::Char),
            TokenKind::Static => specifier.qualifiers.storage = StorageQual::Static,
            TokenKind::Const => specifier.qualifiers.cv = Some(CVQual::Const),
            _ => {return Ok(false);}
        }
        self.get_token();
        Ok(true)
    }

    fn parse_pointers(&mut self) -> Vec<DeclaratorPart> {
        self.eat_token(TokenKind::Star);

        let mut declarator: Vec<DeclaratorPart> = Vec::new();

        declarator.push(DeclaratorPart::PointerDecl(None));

        if self.expect_token(TokenKind::Star) {
            let mut other = self.parse_pointers();
            declarator.append(&mut other);
        }

        declarator
    }
    // Adds a declarator to a Type
    // Add the token name to this thing
    fn parse_declarator(&mut self, mut identifier_found: bool, check_pointers: bool) -> Result<(Vec<DeclaratorPart>, Option<InternedString>), ParserError> {
        /* Convert declarator to direct declarator, by conditionally handling pointers*/
        let mut right_decl: Vec<DeclaratorPart> = Vec::new();
        let mut left_decl: Vec<DeclaratorPart> = Vec::new();

        let mut identifier: Option<InternedString> = None;
        
        if check_pointers && self.expect_token(TokenKind::Star) {
            right_decl = self.parse_pointers();
        }

        if let TokenKind::Identifier(string) = self.peek_token().kind {
            self.get_token();
            identifier_found = true;
            identifier = Some(string);
        }

        else if self.expect_token(TokenKind::OpenParen) {
            self.eat_token(TokenKind::OpenParen)?;
            let (new_declarator, new_identifier) = self.parse_declarator(identifier_found, check_pointers)?;

            if new_identifier.is_some()  {
                if identifier_found == true {
                    let tok = self.prev_token();
                    return Err(ParserError::GeneralError("unexepected identifier".to_string(), Some(tok)));
                }
                else {
                    identifier = new_identifier;
                    identifier_found = true;
                }
            }
            left_decl = new_declarator;
            self.eat_token(TokenKind::CloseParen)?;
        }

        // TODO: Function pointers

        // Open bracket for arrays
        while self.expect_token(TokenKind::OpenBracket) {
            if identifier_found && self.expect_token(TokenKind::OpenBracket) {
                self.eat_token(TokenKind::OpenBracket)?;
                // Constant expressions:
                // Validate that this is a constant expression self.parse_expression(binding_power) 
                //let array_size = self.parse_int_literal()?;

                // Evaluate it in parser for now
                let array_size: usize = match self.get_token().kind {
                    TokenKind::IntLiteral(value) => value.try_into().unwrap(),
                    _ => {return Err(ParserError::ExpectedConstantInt(self.prev_token()));}
                };

                self.eat_token(TokenKind::CloseBracket)?;

                left_decl.push(DeclaratorPart::ArrayDecl(array_size));
                
            }
        }

        left_decl.append(&mut right_decl);
        Ok((left_decl, identifier))
            
    }


    fn parse_function_definition(&mut self, return_type: InternedType, identifier: InternedString) -> Result<ASTNodeHandle, ParserError> {    
        self.eat_token(TokenKind::OpenParen)?;

        let mut parameters: Vec<ASTNodeHandle> = Vec::new();

        while !self.expect_token(TokenKind::CloseParen) {
            let mut specifier: TypeSpecifier = self.parse_declaration_specifiers()?;

            // TODO: Check if there was actually a declaration specifier
            if let (declarator, Some(param_id), ) = self.parse_declarator(false, true)? {
                // else ERROR

                let param_type = self.context.get_type(
                &Type {
                    declarator,
                    specifier,
                });

                let param = ASTNode::ParameterDecl { identifier: param_id, type_info: param_type };
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
                let tok = self.prev_token();
                return Err(ParserError::GeneralError("Expected a name for this variable.".to_string(), Some(tok)))            
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

    fn parse_declaration(&mut self, type_info: InternedType, identifier: InternedString) -> Result<ASTNodeHandle, ParserError> {
        let variable_token = self.prev_token(); // This is so busted.


        if self.expect_token(TokenKind::Equals) {
            self.eat_token(TokenKind::Equals)?;
            // Variable Declaration
            let initializer: ASTNodeHandle = self.parse_expression(0)?;

            self.eat_token(TokenKind::Semicolon)?;

            let node = ASTNode::VariableDecl { identifier: identifier, initializer: Some(initializer), type_info: type_info };

            let node_h = self.ast.nodes.insert(node);
            
            self.context.map_token(node_h, variable_token);
            
            Ok(node_h)

        }
        else {
            self.eat_token(TokenKind::Semicolon)?; 

            let node = ASTNode::VariableDecl { identifier, initializer: None, type_info: type_info };
            let node_h = self.ast.nodes.insert(node);

            self.context.map_token(node_h, variable_token);
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
                // Parse type information
                let specifier = self.parse_declaration_specifiers()?;

                if specifier.ctype == Some(CType::Int) || specifier.ctype == Some(CType::Char) {
                    match self.parse_declarator(false, true)? {
                        (declarator, None) => {
                            let tok = self.peek_token();
                            return Err(ParserError::MissingDeclarator(tok))
                        },
                        (declarator, Some(identifier)) => {

                            let type_info = self.context.get_type(&Type {
                                declarator,
                                specifier,
                            });  

                            self.parse_declaration(type_info, identifier)
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

        if self.expect_token(TokenKind::Semicolon) {
            let stmt = ASTNode::ReturnStmt { expression: None };
            let stmt = self.ast.nodes.insert(stmt);
            self.eat_token(TokenKind::Semicolon)?;
            Ok(stmt)
        }
        else {
            let expression: ASTNodeHandle = self.parse_expression(0)?;
            let stmt = ASTNode::ReturnStmt { expression: Some(expression) };
            let stmt = self.ast.nodes.insert(stmt);
            self.eat_token(TokenKind::Semicolon)?;
            Ok(stmt)
        }
        
    }

    fn parse_if_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::If)?;
        self.eat_token(TokenKind::OpenParen)?;
        let condition = self.parse_expression(0)?;
        self.eat_token(TokenKind::CloseParen)?;
        

        // If part
        let if_branch= if self.expect_token(TokenKind::OpenBrace) {
            self.parse_compound_statement()?
        }
        else {
            let tmp = self.parse_expression(0)?;
            self.eat_token(TokenKind::Semicolon)?;
            tmp
        };

        let else_branch = if self.expect_token(TokenKind::Else) {
            self.eat_token(TokenKind::Else)?;

            let else_stmt= if self.expect_token(TokenKind::OpenBrace) {
                self.parse_compound_statement()?
            }
            else {
                let tmp = self.parse_expression(0)?;
                self.eat_token(TokenKind::Semicolon)?;
                tmp
            };
            Some(else_stmt)

        }
        else {
            None
        };

        let node = ASTNode::IfStmt { condition, if_branch, else_branch };
        Ok(self.ast.nodes.insert(node))

    }

    fn parse_while_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::While)?;
        self.eat_token(TokenKind::OpenParen)?;

        let condition = self.parse_expression(0)?;

        self.eat_token(TokenKind::CloseParen)?;
        let body = self.parse_compound_statement()?;

        let node = ASTNode::WhileStmt { condition, body};
        Ok(self.ast.nodes.insert(node))
    }

    fn parse_for_statement(&mut self) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::For)?;
        self.eat_token(TokenKind::OpenParen)?;
        // TODO: Only certain declaration specifier are allowed
        let initializer = self.parse_for_init_clause()?;

        let condition = if self.expect_token(TokenKind::Semicolon) {
            let node = ASTNode::IntLiteral { value: 1 };
            Ok(self.ast.nodes.insert(node))
        }
        else {
            self.parse_expression(0)
        }?;

        self.eat_token(TokenKind::Semicolon)?;

        let update = self.parse_expression(0)?;

        self.eat_token(TokenKind::CloseParen)?;

        let body = self.parse_compound_statement()?;

        let node = ASTNode::ForStmt { initializer, condition, update, body};
        Ok(self.ast.nodes.insert(node))


    }

    fn parse_for_init_clause(&mut self) -> Result<ASTNodeHandle, ParserError> {
        let specifier = self.parse_declaration_specifiers()?;
        if specifier.ctype == Some(CType::Int) {
            let (declarator, identifier) = self.parse_declarator(false, true)?;

            let identifier = if identifier.is_none() {
                return Err(ParserError::MissingDeclarator(self.prev_token()));
            } else {
                identifier.unwrap()
            };

            let type_info = self.context.get_type(&Type {
                declarator,
                specifier,
            });
            
            self.parse_declaration(type_info, identifier)
        }
        else {
            let expr = self.parse_expression(0);
            self.eat_token(TokenKind::Semicolon);
            expr
        }
        
    }

    fn parse_struct(&mut self) -> Result<ASTNodeHandle, ParserError> {
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
                    let token = self.peek_token();
                    return Err(ParserError::GeneralError("Expected expression.".to_string(), Some(token)))
                }
            }
        };

        // Infix and postfix expressoins:
        loop {
            if self.expect_token(TokenKind::Semicolon) || self.expect_token(TokenKind::CloseParen) || self.expect_token(TokenKind::CloseBracket) {
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
                    lhs = self.parse_function_call(lhs)?;
                }
                else if token.kind == TokenKind::OpenBracket {
                    self.eat_token(TokenKind::OpenBracket)?;
                    let rhs = self.parse_expression(0)?;
                    self.eat_token(TokenKind::CloseBracket)?;
                    let node = ASTNode::BinaryOp { op: BinaryOpType::ArrayAccess, right: rhs, left: lhs };
                    lhs = self.ast.nodes.insert(node);
                }
                // Handle function calls somewhere
                else {
                    self.get_token(); 
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
                    TokenKind::LessThan => BinaryOpType::LessThan,
                    TokenKind::LessThanEqual => BinaryOpType::LessThanEqual,
                    TokenKind::GreaterThan => BinaryOpType::GreaterThan,
                    TokenKind::GreaterThanEqual => BinaryOpType::GreaterThanEqual,
                    TokenKind::EqualsEquals => BinaryOpType::EqualEqual,
                    TokenKind::ExclamationEquals => BinaryOpType::NotEqual,
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
            let token = self.get_token();
            
            let node = ASTNode::SymbolRef {identifier: identifier};
            
            // TOOD: Write helper function that takes node and token and does this for you:
            let node_h = self.ast.nodes.insert(node);
            self.context.map_token(node_h, token);
            Ok(node_h)
        }
        else {
            return Err(ParserError::GeneralError("Expected symbol.".to_string(), Some(self.prev_token())))
        }
        
    }

    fn parse_function_call(&mut self, symbol_ref: ASTNodeHandle) -> Result<ASTNodeHandle, ParserError> {
        self.eat_token(TokenKind::OpenParen);
        let mut arguments = Vec::new();

  
        loop {
            // Expect Close Paren
            if self.expect_token(TokenKind::CloseParen) {
                break;
            }

            // Parse expression,
            let arg = self.parse_expression(0)?;
            arguments.push(arg);

            if self.expect_token(TokenKind::CloseParen) {
                break;
            }

            else {
                self.eat_token(TokenKind::Comma)?;
            }
        }
        
        self.eat_token(TokenKind::CloseParen)?;
        let node = ASTNode::FunctionCall { symbol_ref, arguments};
        Ok(self.ast.nodes.insert(node))
    }
    
}

/* 
#[cfg(test)]
mod parser_tests {

    use std::cell::RefCell;
    use std::mem::{Discriminant, discriminant};
    use std::rc::Rc;

    use crate::ast::{Vistior, ASTCheck, ASTPrint};
    use crate::context;
    use crate::lexer::{Lexer};
    use crate::parser::{Parser};
    use super::*;
    

    #[test]
    fn var_decl() {
        let src = String::from("int a;");

        let context = Context::new();
        
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str(), error_handler.clone());
        
        let mut parser: Parser<'_> = Parser::new(& mut lexer, error_handler);

        let root = parser.parse_translation_unit().unwrap();
        let ast: &AST = &parser.ast;

        let mut checker: ASTCheck<'_> = ASTCheck::new(ast); 

        checker.traverse(&root);

        
        let gold: Vec<Discriminant<ASTNode>> = vec![
            discriminant(&ASTNode::Program { declarations: Vec::new()}),
            discriminant(&ASTNode::VariableDecl { identifier: Strings.lock().unwrap().get_or_intern("hi") , initializer: None, type_info: Type::new() }),
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
            discriminant(&ASTNode::VariableDecl() { identifier: Token::default(), initializer: None, type_info: Type::default() }),
        ];

        for i in 0..checker.results.len() {
            dbg!(checker.results[i]);
            assert_eq!(checker.results[i], gold[i])
        }
        */
    }
    
}*/