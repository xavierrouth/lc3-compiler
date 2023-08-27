

use lex_parse::ast::{Vistior, ASTNode, TraversalOrder};
use lex_parse::token::Token;
use lex_parse::types::TypeInfo;

pub struct STScope {
    entries: Vec<STEntry>,
    parent: Box<STScope>,
}

impl STScope {
    pub fn new(parent: Box<STScope>) -> STScope {
        STScope {
            entries: Vec::new(),
            parent,
        }
    }
}

enum STEntryType {
    ParameterEntry,
    VariableEntry,
    FunctionEntry,
}

pub struct STEntry {
    size: usize,
    offset: usize,
    type_info: TypeInfo
} 


pub struct Analyzer<'a> {
    scopes_stack: Vec<&'a Box<STScope>>, // Reference to a STScope
    entries_map: SlotMap<ASTNodeHandle, ASTNode<'a>>, // 
}

/*/
impl <'a> Vistior<'a> for Analyzer<'a> {
    fn get_order(&self) -> TraversalOrder {
        TraversalOrder::PreOrder
    }

    fn entry(&mut self, node: &ASTNode<'a>) -> () {
        STScope::new()
        let new_scope: STScope = STScope::new(self.scopes_stack.last().to_owned().unwrap());
        self.
    }

    fn operate(&mut self, node: &ASTNode<'a>) -> () {
        match node {
            ASTNode::
        }
    }
}
*/