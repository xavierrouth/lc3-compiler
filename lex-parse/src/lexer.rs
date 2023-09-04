use core::{fmt};
use std::{error, cell::RefCell, rc::Rc};

use colored::Colorize;

use crate::{token::{Token, TokenKind, self}, context::{InternedString, Context}, error::{LexerError, ErrorHandler}};

pub struct Lexer<'a, 'ctx> {
    index: usize,
    row: usize,
    col: usize,
    putback: char,

    input_stream: &'a str,
    context: &'a Context<'ctx>,
    error_handler: &'a ErrorHandler<'a>
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a, 'ctx> Lexer<'a, 'ctx> {

    pub fn new(input_stream: &'a str, context: &'a Context<'ctx>, error_handler: &'a ErrorHandler<'a>) -> Lexer<'a, 'ctx> {
        Lexer {
            index: 0,
            row: 0,
            col: 0,
            input_stream,
            putback: EOF_CHAR,

            context,
            error_handler,
        }
    }

    pub fn get_token(&mut self) -> Result<Token, LexerError> {
       
        self.skip_until();
        
        let start_index = self.index;

        let row = self.row;
        let col = self.col;
        let mut ch = self.next();

        // There must be a better way to do this.
        // Match Tokens:
        let kind: Result<TokenKind, LexerError> = match ch  {
            '!' => {ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::ExclamationEquals),
                _ => {self.putback(ch); Ok(TokenKind::Exclamation)}
            }},
            '&' => { ch = self.next(); 
                match ch {
                '&' => Ok(TokenKind::AndAnd),
                '=' => Ok(TokenKind::AndEquals),
                 _ => {self.putback(ch); Ok(TokenKind::And)}
            }},
            '|' => { ch = self.next(); 
                match ch {
                '|' => Ok(TokenKind::BarBar),
                '=' => Ok(TokenKind::BarEquals),
                _ => {self.putback(ch); Ok(TokenKind::Bar)}
            }},
            '=' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::EqualsEquals),
                _ => {self.putback(ch); Ok(TokenKind::Equals)}
            }},
            '%' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::PercentEquals),
                _ => {self.putback(ch); Ok(TokenKind::Percent)}
            }},
            '/' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::SlashEquals),
                '/' => {self.skip_line(); return self.get_token()}, // If these are our output, then do extra kjelkj before returning a token. 
                '*' => {self.skip_block_comment(); return self.get_token()}
                _ => {self.putback(ch); Ok(TokenKind::Slash)}
            }},
            '-' => { ch = self.next(); 
                match ch {
                '-' => Ok(TokenKind::MinusMinus),
                '>' => Ok(TokenKind::Arrow), 
                '=' => Ok(TokenKind::MinusEquals),
                _ => {self.putback(ch); Ok(TokenKind::Minus)}
            }},
            '+' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::PlusEquals),
                '+' => Ok(TokenKind::PlusPlus), 
                _ => {self.putback(ch); Ok(TokenKind::Plus)}
            }},
            '*' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::StarEquals),
                _ => {self.putback(ch); Ok(TokenKind::Star)}
            }},
            '<' => { ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::LessThanEqual),
                '<' => {  ch = self.next();
                    match ch {
                        '=' => Ok(TokenKind::LeftShiftEquals),
                        _ => {self.putback(ch); Ok(TokenKind::LeftShift)}
                    }
                }
                _ => {self.putback(ch); Ok(TokenKind::LessThan)}
            }},
            '>' => {ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::GreaterThanEqual),
                '>' => {ch = self.next();
                    match ch {
                        '=' => Ok(TokenKind::RightShiftEquals),
                        _ => {self.putback(ch); Ok(TokenKind::RightShift)}
                    }
                }
                _ => {self.putback(ch); Ok(TokenKind::GreaterThan)}
            }},
            '^' => {ch = self.next(); 
                match ch {
                '=' => Ok(TokenKind::CaretEquals),
                _ => {self.putback(ch); Ok(TokenKind::Caret)}
            }},
            '"' => {
                Ok(TokenKind::StringLiteral(self.string_literal().unwrap()))
            }
            EOF_CHAR => Ok(TokenKind::EOF),
            '~' => Ok(TokenKind::Tilde),
            '(' => Ok(TokenKind::OpenParen),
            ')' => Ok(TokenKind::CloseParen),
            '[' => Ok(TokenKind::OpenBracket),
            ']' => Ok(TokenKind::CloseBracket),
            '{' => Ok(TokenKind::OpenBrace),
            '}' => Ok(TokenKind::CloseBrace),
            ';' => Ok(TokenKind::Semicolon),
            ',' => Ok(TokenKind::Comma),
            '?' => Ok(TokenKind::Question),
            ':' => Ok(TokenKind::Colon),
            '.' => Ok(TokenKind::Dot),
            _ if ch.is_ascii_digit() => {
                // Don't support floats for now
                Ok(TokenKind::IntLiteral(self.number(ch).unwrap_or(0)))
            }
            _ if ch.is_ascii_alphanumeric() || ch == '_' => {
                let string = self.symbol(ch).unwrap();
                
                match self.context.resolve_string(string).as_str() {
                    "auto" => Ok(TokenKind::Auto),
                    "break" => Ok(TokenKind::Break),
                    "int" => Ok(TokenKind::Int),
                    "return" => Ok(TokenKind::Return),
                    "void" => Ok(TokenKind::Void),
                    "static" => Ok(TokenKind::Static),
                    "if" => Ok(TokenKind::If),
                    "else" => Ok(TokenKind::Else),
                    "for" => Ok(TokenKind::For),
                    "while" => Ok(TokenKind::While),
                    "do" => Ok(TokenKind::Do),
                    "const" => Ok(TokenKind::Const),
                    // TODO:
                    _  => Ok(TokenKind::Identifier(string))
                }
                
            }
            _ => {
                Err(LexerError::UnknownError)
            }

        };


        let length: usize = self.index - start_index;

        let token: Token = Token {kind: kind.unwrap_or(TokenKind::EOF), row, col, length, };

        Ok(token)
    }

    fn skip_line(&mut self) -> () {
        let mut ch: char = self.next();
        while ch != EOF_CHAR && ch != '\n' {
            ch = self.next();
        }
    }

    fn skip_block_comment(&mut self) -> () {
        loop {
            let ch: char = self.next();
            if ch == EOF_CHAR {
                println!("{} Expected */ to end block comment.", "error:".red());
            }
            if ch == '*' {
                if self.next() == '/' {
                    return;
                }
            }
            continue;
        }
    }   

    fn number(&mut self, mut ch: char) -> Result<i32, ()> {
        let _float_error: bool = false;
        let mut val: i32 = ch as i32 - '0' as i32;

        ch = self.next();

        while ch.is_ascii_digit() {
            val = val * 10;
            val = val + ch as i32 - '0' as i32;
            
            ch = self.next();
        }

        self.putback(ch);
        Ok(val)
    }

    fn symbol(&mut self, mut ch: char) -> Result<InternedString, ()> {
        let mut str: String = String::new();
        while ch.is_ascii_alphanumeric() || ch.is_ascii_digit() || ch == '_' {
            str.push(ch);
            ch = self.next();
        }
        self.putback(ch);
        
        Ok(self.context.get_string(&str))
    }

    fn string_literal(&mut self) -> Result<InternedString, ()> {
        let mut str: String = String::new();
        let mut ch:char = self.next();
        while ch  != '"' {
            str.push(ch);
            ch = self.next();
        }
        Ok(self.context.get_string(&str))
    }

    fn next(&mut self) -> char {
        if self.putback != EOF_CHAR {
            let ch = self.putback;
            self.putback = EOF_CHAR;
            ch
        }
        else {
            let ch = match self.input_stream.chars().nth(0) {
                Some(ch) => ch,
                None => return EOF_CHAR // This is EOF 
            };
            self.col += 1;

            self.index += ch.len_utf8();

            if ch == '\n' {
                self.advance();
                self.col = 0;
                self.row += 1;
            }

            self.advance();
            ch
        }

    }

    fn putback(&mut self, ch: char) -> () {
        self.putback = ch;
    }

    fn skip_until(&mut self) -> () {
        let mut ch: char = self.next();

        while ch.is_ascii_whitespace() {
            ch = self.next();
        }
        self.putback(ch);
    } 

    fn advance(&mut self) -> () {
        self.input_stream = &self.input_stream[self.index..];
        self.index = 0;
    }


}


#[cfg(test)]
mod lexer_tests {

    use std::cell::RefCell;
    use std::rc::Rc;
    use std::mem;

    use string_interner::StringInterner;
    use string_interner::symbol::SymbolU16;

    use crate::error::ErrorHandler;
    use crate::lexer::{Lexer};
    use crate::token::{TokenKind};
    use crate::context::{Context, InternedString};
    use crate::token::TokenKind::*;

    macro_rules! lexer_test {
        ($name:ident, $input:literal, $gold:expr) => {
            #[test]
            fn $name() {

                let input_stream = String::from($input);

                let context = Context::new(&input_stream);
                let error_handler: ErrorHandler<'_> = ErrorHandler::new(&context);
                let mut lexer: Lexer<'_, '_> = Lexer::new(&input_stream, &context, &error_handler);

                for g in $gold {
                    let t = lexer.get_token().unwrap().kind;
                    assert_eq!(mem::discriminant(&t), mem::discriminant(&g));
                }
            }
        }
    }
    //static sym: SymbolU16 = SymbolU16 {value: std::num::NonZeroU16::new(0).unwrap()};
    //static Identifier: TokenKind = TokenKind::Identifier(sym);
    // Wtf is this:
    //static IDENTIFIER: TokenKind = TokenKind::Identifier(SymbolU16 {value: unsafe { std::num::NonZeroU16::new_unchecked(0) }});
    lazy_static! {
        static ref IDENTIFIER: TokenKind = {
            let c = Context::new("");
            let t = TokenKind::Identifier(c.get_string("main"));
            t
        };
    }

    lexer_test!(basic, "+ = -", vec![Plus, Equals, Minus, EOF]);
    lexer_test!(asdawd, "2 1230 1238", vec![IntLiteral(0), IntLiteral(0), IntLiteral(0), EOF]);

    // Impossible to test?
    lexer_test!(wdasd, "rghjekshgrklj wdjawlkfj gregreklj", vec![IDENTIFIER.clone(), IDENTIFIER.clone(), IDENTIFIER.clone(), EOF]);
    
}