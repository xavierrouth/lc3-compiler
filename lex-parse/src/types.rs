#[derive(Debug, Clone, PartialEq)]
pub struct SpecifierInfo {
    pub marked_const: bool,
    pub marked_volatile: bool,
    pub marked_static: bool,
    pub marked_int: bool,
    pub marked_char: bool,
    pub marked_void: bool
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclaratorPart {
    FunctionDecl(SpecifierInfo),
    PointerDecl(Option<SpecifierInfo>),
    ArrayDecl(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub declarator: Vec<DeclaratorPart>,
    pub type_specifier: SpecifierInfo,
    pub identifier: Option<String>,
}



