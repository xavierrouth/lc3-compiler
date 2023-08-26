use std::error;
use std::fmt;

use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    index: usize,
    row: i32,
    col: i32,
    input_stream: &'a str,
    putback: char,
}

#[derive(Debug)]
pub enum LexerError {
    FloatError(String),
    UnknownError
}

impl error::Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::FloatError(msg) => write!(f, "{}", msg),
            LexerError::UnknownError => write!(f, "Something went wrong"),
        }
    }
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {

    pub fn new(src: &str) -> Lexer<'_> {
        Lexer {
            index: 0,
            row: 0,
            col: 0,
            input_stream: src,
            putback: EOF_CHAR,
        }
    }

    pub fn get_token(&mut self) -> Result<Token<'a>, ()> {
       
        let mut ch: char = self.skip_until();
        
        let start_index = self.index;

        let row = self.row;
        let col = self.col;
        // There must be a better way to do this.
        // Match Tokens:
        let kind: Result<TokenKind<'a>, LexerError> = match ch  {
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
                '/' => Ok(TokenKind::SlashSlash), // If these are our output, then do extra kjelkj before returning a token. 
                '*' => Ok(TokenKind::SlashStar),
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
                Ok(TokenKind::StringLiteral(self.string_literal().unwrap_or(String::from("oops!"))))
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
                Ok(TokenKind::Identifier(self.symbol(ch).unwrap_or(String::from("oops!"))))
            }
            _ => {
                Err(LexerError::UnknownError)
            }

        };

        let length: usize = self.index - start_index;

        let token: Token<'a> = Token {kind: kind.unwrap_or(TokenKind::EOF), row, col, length};

        Ok(token)
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

    fn symbol(&mut self, mut ch: char) -> Result<String, ()> {
        let mut str: String = String::new();
        while ch.is_ascii_alphanumeric() || ch.is_ascii_digit() || ch == '_' {
            str.push(ch);
            ch = self.next();
        }
        Ok(str)
    }

    fn string_literal(&mut self) -> Result<String, ()> {
        let mut str: String = String::new();
        let mut ch:char = self.next();
        while ch  != '"' {
            str.push(ch);
            ch = self.next();
        }
        Ok(str)
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

            if ch == '\n' {
                self.row = 0;
                self.col += 1;
            }
            self.advance_char(ch);
            ch
        }
    }

    fn putback(&mut self, ch: char) -> () {
        self.putback = ch;
    }

    fn skip_until(&mut self) -> char {
        let mut ch: char = self.next();

        while ch.is_ascii_whitespace() {
            ch = self.next();
        }
        ch
    } 

    fn advance_char(&mut self, ch: char) -> () {
        self.advance(ch.len_utf8());
    }

    fn advance(&mut self, bytes: usize) -> () {
        self.index += bytes;
        self.input_stream = &self.input_stream[bytes..]; // Advance input stream by num_bytes
    }

}


#[cfg(test)]
mod lexer_tests {

    use crate::lexer::{Lexer};
    use crate::token::{TokenKind};

    #[test]
    fn basic() {
        let src = String::from("+ = -");
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str());
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Plus);
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Equals);
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Minus);
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn symbol() {
        let src = String::from("test hi ethan");
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str());
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Identifier("test".to_string()));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Identifier("hi".to_string()));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::Identifier("ethan".to_string()));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn integer() {
        let src = String::from("3043 423423 120");
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str());
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(3043));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(423423));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(120));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn float() {
        let src = String::from("3043.434 423423.543 120.654");
        let mut lexer: Lexer<'_> = Lexer::new(src.as_str());
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(3043));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(423423));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::IntLiteral(120));
        assert_eq!(lexer.get_token().unwrap().kind, TokenKind::EOF);
    }
    
}