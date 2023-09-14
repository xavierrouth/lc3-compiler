use core::{fmt};
use std::{error, cell::RefCell, rc::Rc};
use std::str::Chars;

use colored::Colorize;

use crate::{token::{Token, TokenKind}, context::{InternedString, Context}, error::{LexerError, ErrorHandler}};


pub(crate) const EOF_CHAR: char = '\0';

// I give up, this is not an interesting problem, therefore:
// Heavily inspired by rustc's lexer.
pub struct Lexer<'a, 'ctx> {
    row: usize,
    col: usize,
    remaining: usize,

    putback: char,

    chars: Chars<'a>,
    context: &'a Context<'ctx>,
    error_handler: &'a ErrorHandler<'a>
}


impl<'a, 'ctx> Lexer<'a, 'ctx> {
    pub fn new(input: &'a str, context: &'a Context<'ctx>, error_handler: &'a ErrorHandler<'a>) -> Lexer<'a, 'ctx> {
        Lexer {
            row: 0,
            col: 0,

            chars: input.chars(),
            remaining: input.len(),
            putback: EOF_CHAR,

            context,
            error_handler,
        }
    }

    fn first(&self) -> char {
        self.chars.clone().nth(0).unwrap_or(EOF_CHAR)
    }

    fn second(&self) -> char {
        self.chars.clone().nth(1).unwrap_or(EOF_CHAR)
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        match c {
            '\n' => {
                self.col = 0;
                self.row += 1;
            },
            _ => {
                self.col += 1;
            }
        }
        Some(c)
    }

    fn reset_pos(&mut self) -> () {
        self.remaining = self.chars.as_str().len()
    }

    fn get_pos(&self) -> u32 {
        (self.remaining - self.chars.as_str().len()) as u32
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool)  -> () {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    pub fn get_token(&mut self) -> Result<Token, LexerError> {
        

        self.whitespace();
        self.reset_pos(); // For recursive comment calls.

        let first = match self.bump() {
            Some(c) => c,
            None => return Ok(Token { kind: TokenKind::EOF, row: 0, col: 0, length: 0 }) 
        };
        
        let row = self.row;
        let col = self.col;

        // There must be a better way to do this.
        // Match Tokens:
        let kind: Result<TokenKind, LexerError> = match first  {
            '!' => { match self.first() {
                '=' => {self.bump();Ok(TokenKind::ExclamationEquals)},
                _ =>  Ok(TokenKind::Exclamation),
            }},
            '&' => { match self.first() {
                '&' => {self.bump();Ok(TokenKind::AndAnd)},
                '=' => {self.bump();Ok(TokenKind::AndEquals)},
                 _ => Ok(TokenKind::And),
            }},
            '|' => { match self.first() {
                '|' => {self.bump();Ok(TokenKind::BarBar)},
                '=' => {self.bump();Ok(TokenKind::BarEquals)},
                _ =>  Ok(TokenKind::Bar),
            }},
            '=' => { match self.first() {
                '=' => {self.bump();Ok(TokenKind::EqualsEquals)},
                _ =>  Ok(TokenKind::Equals)
            }},
            '%' => { match self.first() {
                '=' => {self.bump(); Ok(TokenKind::PercentEquals)},
                _ => Ok(TokenKind::Percent)
            }},
            '/' => { match self.first() {
                '=' => {self.bump(); Ok(TokenKind::SlashEquals)},
                '/' => {self.line_comment(); return self.get_token()}, // If these are our output, then do extra kjelkj before returning a token. 
                '*' => {self.block_comment(); return self.get_token()}
                _ => Ok(TokenKind::Slash)
            }},
            '-' => { match self.first() {
                '-' => {self.bump(); Ok(TokenKind::MinusMinus)},
                '>' => {self.bump(); Ok(TokenKind::Arrow)}, 
                '=' => {self.bump(); Ok(TokenKind::MinusEquals)},
                _ => Ok(TokenKind::Minus)
            }},
            '+' => { match self.first() {
                '=' => {self.bump(); Ok(TokenKind::PlusEquals)},
                '+' => {self.bump(); Ok(TokenKind::PlusPlus)}, 
                _ => Ok(TokenKind::Plus)
            }},
            '*' => { match self.first() {
                '=' => {self.bump(); Ok(TokenKind::StarEquals)},
                _ => Ok(TokenKind::Star)
            }},
            '<' => { match self.first() {
                '=' => {self.bump(); Ok(TokenKind::LessThanEqual)},
                '<' => { match self.second() {
                        '=' => {self.bump(); self.bump(); Ok(TokenKind::LeftShiftEquals)},
                        _ => {self.bump(); Ok(TokenKind::LeftShift)}
                    }
                }
                _ => Ok(TokenKind::LessThan)
            }},
            '>' => {match self.first() {
                '=' => {self.bump(); Ok(TokenKind::GreaterThanEqual)},
                '>' => {match self.second() {
                        '=' => {self.bump(); self.bump(); Ok(TokenKind::RightShiftEquals)},
                        _ => {self.bump(); Ok(TokenKind::RightShift)}
                    }
                }
                _ => Ok(TokenKind::GreaterThan)
            }},
            '^' => {match self.first() {
                '=' => {self.bump(); Ok(TokenKind::CaretEquals)},
                _ => Ok(TokenKind::Caret)
            }},
            '"' => {
                panic!();
                //Ok(TokenKind::StringLiteral(self.string_literal().unwrap()))
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
            c if c.is_ascii_digit() => {
                Ok(TokenKind::IntLiteral(self.number(c).unwrap_or(0)))
            }
            c if c.is_ascii_alphanumeric() || c == '_' => {
                let string = self.symbol(c).unwrap();
                
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
                    "struct" => Ok(TokenKind::Struct),
                    "NULL" => Ok(TokenKind::Null),
                    // TODO:
                    _  => Ok(TokenKind::Identifier(string))
                }
            }
            _ => {
                Err(LexerError::UnknownError)
            }

        };

        let length = self.get_pos();
        self.reset_pos();

        let token: Token = Token {kind: kind.unwrap_or(TokenKind::EOF), row, col, length: length.try_into().unwrap()}; // I love rust.

        Ok(token)
    }

    fn line_comment(&mut self) -> () {
        self.eat_while(|c| c != '\n');
    }

    fn whitespace(&mut self) -> () {
        self.eat_while(|c| c.is_ascii_whitespace())
    } 

    fn block_comment(&mut self) -> () {
        self.bump();

        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    break;
                }
                EOF_CHAR => {
                    println!("{} Expected */ to end block comment.", "error:".red());
                }
                _ => (),
            }
        }
    }   

    fn number(&mut self, first_digit: char) -> Result<i32, ()> {
        let _float_error: bool = false;
        let mut val: i32 = first_digit as i32 - '0' as i32;

        loop {
            let c = self.first();
            match c {
                '0'..='9' => {
                    val = val * 10;
                    val = val + c as i32 - '0' as i32;
                    self.bump();
                }
                _ => break
            }
        }
        Ok(val)
    }

    fn symbol(&mut self, frist_char: char) -> Result<InternedString, ()> {
        let mut str: String = String::new();
        str.push(frist_char);
        loop {
            let c = self.first();
            match c {
                c if c.is_ascii_alphanumeric() || c == '_' => {
                    str.push(c);
                    self.bump();
                    continue;
                }
                _ => 
                    break
            }
        }
        Ok(self.context.get_string(&str))
    }

    /*
    fn string_literal(&mut self) -> Result<InternedString, ()> {
        let mut str: String = String::new();
        let mut ch:char = self.next();
        while ch  != '"' {
            str.push(ch);
            ch = self.next();
        }
        Ok(self.context.get_string(&str))
    }  */


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
                    let t = lexer.get_token().unwrap();
                    println!("{:?}", t);
                    let t = t.kind;
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

    lexer_test!(member_access, "member->access", vec![IDENTIFIER.clone(), Arrow, IDENTIFIER.clone(), EOF]);

    lexer_test!(wdasd, "rghjekshgrklj wdjawlkfj gregreklj", vec![IDENTIFIER.clone(), IDENTIFIER.clone(), IDENTIFIER.clone(), EOF]);
    
}