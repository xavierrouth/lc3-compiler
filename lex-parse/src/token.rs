use std::marker::PhantomData;

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

    Identifier(String),
    IntLiteral(i32),
    StringLiteral(String),

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

    // Helper
    EOF
}
#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub row: i32,
    pub col: i32,
    pub length: usize, // Unused
}