#[derive(Debug, Clone, PartialEq)]
pub struct SpecifierInfo {
    pub marked_const: bool,
    pub marked_volatile: bool,
    pub marked_static: bool,
    pub marked_int: bool,
    pub marked_char: bool,
    pub marked_void: bool
}

impl Default for SpecifierInfo {
    fn default() -> Self {
        SpecifierInfo { marked_const: false, marked_volatile: false, marked_static: false, marked_int: false, marked_char: false, marked_void: false }
    }
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
}

impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo {
            declarator: Vec::new(),
            type_specifier: SpecifierInfo::default(),
        }
    }
}



