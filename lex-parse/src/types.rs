use core::fmt;

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

impl fmt::Display for SpecifierInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tmp: String = String::new();
        if self.marked_const {
            tmp.push_str("const");
        }
        if self.marked_volatile {
            tmp.push_str("volatile");
        }
        if self.marked_static {
            tmp.push_str("static");
        }
        if self.marked_int{
            tmp.push_str("int");
        }
        if self.marked_char {
            tmp.push_str("char");
        }
        if self.marked_void {
            tmp.push_str("void");
        }
        write!(f, "{tmp}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclaratorPart {
    FunctionDecl(SpecifierInfo),
    PointerDecl(Option<SpecifierInfo>),
    ArrayDecl(usize),
}

impl DeclaratorPart {
    pub fn size(&self) -> usize {
        match self {
            DeclaratorPart::FunctionDecl(_) => 1,
            DeclaratorPart::PointerDecl(_) => 1,
            DeclaratorPart::ArrayDecl(size) => *size,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub declarator: Vec<DeclaratorPart>,
    pub type_specifier: SpecifierInfo,
}

impl TypeInfo {
    pub fn calculate_size(&self) -> usize {
        // Don't do bytes vs other stuff for now
        match self.declarator.first() {
            Some(decl) => decl.size(),
            None => 1
        }
    }
}

impl fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        
        fn print_declarator(declarator: &mut Vec<DeclaratorPart>, prev_fnc_or_array: bool) -> String {
            let mut buffer: String = String::new();
            let part = declarator.pop();

            if part.is_none() {
                return buffer;
            }
            let part = part.unwrap();

            match part {
                DeclaratorPart::FunctionDecl(_) => {todo!()}
                DeclaratorPart::ArrayDecl(size) => {
                    buffer.push_str(&print_declarator(declarator, true));
                    buffer.push_str(&format!("[{size}]"));
                    buffer
                }
                DeclaratorPart::PointerDecl(part) => {
                    if prev_fnc_or_array {
                        buffer.push('(');
                    }
                    buffer.push('*');
                    buffer.push_str(&print_declarator(declarator, false));
                    if prev_fnc_or_array {
                        buffer.push(')');
                    }
                    buffer
                }
            }
        }
        let string = print_declarator(& mut self.declarator.clone(), false);

        write!(f, "{:} {string}", self.type_specifier)
    }
    
}

impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo {
            declarator: Vec::new(),
            type_specifier: SpecifierInfo::default(),
        }
    }
}



