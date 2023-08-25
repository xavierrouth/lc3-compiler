pub struct SpecifierInfo {
    marked_const: bool,
    marked_volatile: bool,
    marked_static: bool,
    marked_int: bool,
    marked_char: bool,
    marked_void: bool
}

pub enum DeclaratorPart {
    FunctionDecl(SpecifierInfo),
    PointerDecl(Option<SpecifierInfo>),
    ArrayDecl(i32),
}

pub struct TypeInfo {
    declarator: Vec<DeclaratorPart>,
    type_specifier: SpecifierInfo,
}



