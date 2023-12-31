use crate::context::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    AndAnd,
    BarBar,
    Exclamation,
    PlusPlus,
    MinusMinus,
    And,
    Bar,
    Caret,
    Tilde,
    LeftShift,
    RightShift,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    EqualsEquals,
    ExclamationEquals,
    Semicolon,
    Colon,
    Comma,

    Dot,
    Arrow,
    Question,

    LeftShiftEquals,
    RightShiftEquals,
    StarEquals,
    PlusEquals,
    MinusEquals,
    SlashEquals,
    PercentEquals,
    AndEquals,
    CaretEquals,
    BarEquals,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,

    OpenBrace,
    CloseBrace,

    Identifier(InternedString),
    IntLiteral(i32),
    StringLiteral(InternedString),

    // How to handle comments?
    SlashSlash,
    SlashStar,

    // ====== Keywords ======
    Const, 
    Volatile,
    Extern,
    Static,
    Auto,
    Register,
    Typedef,
    Unsigned,
    Signed,
    Float,
    Double,

    Return,
    If,
    For,
    While,
    Break,
    Else,
    Do,

    Int,
    Void,
    Char,
    
    Struct,
    Union,

    // ===== Custom =====
    Null,

    // Helper
    EOF
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub row: usize,
    pub col: usize,
    pub length: usize, // Unused
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::EOF,
            row: 0,
            col: 0,
            length: 0,
        }
    }
}