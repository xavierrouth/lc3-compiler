use core::{fmt, panic};
use std::fmt::Display;


use slotmap::{SparseSecondaryMap, SecondaryMap, SlotMap};

use lex_parse::ast::{Vistior, AST, ASTNode, ASTNodeHandle, BinaryOpType, ASTNodePrintable, UnaryOpType};
use lex_parse::error::{ErrorHandler, AnalysisError}; // Messed up
use lex_parse::context::{Context, InternedType, InternedString};
use lex_parse::types::{Type, DeclaratorPart, TypeSpecifier, StorageQual, Qualifiers, BaseType, TypePrintable};

use crate::typedast::{TypedAST, TypedASTNode, TypedASTNodeHandle};
use crate::symtab::{SymbolTable, VarDecl, ScopeHandle, Scope};


pub struct TypecheckPass<'a> {
    symbol_table: SymbolTable, // 
    ast: AST,

    // Result:
    typed_ast: TypedAST,



    // Internal State
    scopes: SparseSecondaryMap<ASTNodeHandle, ScopeHandle>,
    halt: bool,

    context: &'a Context<'a> //TODO: Merge error handler and context.
}


#[derive(Debug, Clone, PartialEq)]
pub enum LR {
    LValue,
    RValue,
}


impl <'a> TypecheckPass<'a> { 
    pub fn new(ast: AST, symbol_table: SymbolTable, context: &'a Context<'a>, scopes: SparseSecondaryMap<ASTNodeHandle, ScopeHandle>,) -> TypecheckPass<'a> {
        TypecheckPass { symbol_table, ast, typed_ast: TypedAST { nodes: SlotMap::with_key(), root: None }, scopes, halt: false, context, }
    }

    fn check_program(&mut self, node_h: ASTNodeHandle) -> TypedASTNodeHandle {
        let node = self.ast.remove(node_h);

        let mut globals = Vec::new();
        let mut functions = Vec::new();

        let ASTNode::Program { declarations } = node else {
            panic!("invalid node for check program");
        };

        for decl_h in declarations {
            let decl = self.ast.remove(decl_h);
            match decl {
                ASTNode::VariableDecl { identifier, initializer, type_info } => {
                    // TODO:
                    // Make sure initializer is const, reduce to an i32, and then just get a decl from the symbol table??
                    // this is SO messy and gross I hate it, store scope binding somewhere else.
                    // This should always be the global scope, 
                    let scope = self.scopes.get(node_h).expect("invalid node to scope mapping");
                    let decl = self.symbol_table.search_scope(scope, &identifier).expect("can't find decl");
                    globals.push(decl.clone());
                },
                ASTNode::DeclStmt { declarations } => {
                    todo!(); // Decl Statments aren't actually used yet, right now they are just vardecls or something.
                },
                ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                    // Frustrating wasted (union/enum)-tag dispatch, will compiler find some way to optimize?
                    let func = self.check_function(decl_h);
                    functions.push(func);
                },
                ASTNode::RecordDecl {..} => {
                    // What needs to be done here?
                    todo!();
                }
                _ => panic!("unexpected top level node")
            }
        }
        
        let node = TypedASTNode::Program { functions, globals };
        self.typed_ast.nodes.insert(node)

    }

    fn check_function(&mut self, node_h: ASTNodeHandle) -> TypedASTNodeHandle {
        let node = self.ast.remove(node_h);

        let ASTNode::FunctionDecl { body, parameters, identifier, return_type } = node else {
            panic!("invalid node for check function");
        };

        let mut locals = Vec::new();
        let mut typed_parameters = Vec::new();

        for param_h in parameters {
            let param = self.ast.remove(param_h);
            let ASTNode::ParameterDecl { identifier, type_info } = param else {
                panic!("invalid node for check funciton parameter");
            };
            // Extract the type information from the symbol table.
            let scope = self.scopes.get(param_h).expect("invalid node to scope mapping");
            let decl = self.symbol_table.search_scope(scope, &identifier).expect("can't find decl");
            typed_parameters.push(decl.clone());
        }

        /* Typecheck the function body, while adding local variable declarations to the locals array */
        let body = self.check_statement(&mut locals, body);

        /* Build the final TypedASTNode */
        let node = TypedASTNode::FunctionDecl { body, identifier, return_type, parameters: typed_parameters, locals };
        self.typed_ast.nodes.insert(node)
    }

    fn check_statement(&mut self, locals: &mut Vec<VarDecl>, node_h: ASTNodeHandle) -> TypedASTNodeHandle {
        let node = self.ast.remove(node_h);

        match node {
            ASTNode::VariableDecl { identifier, initializer, type_info } => {
                // TODO: Maintain mapping between declaration in C and where this is going to get lowered to.
                let scope = self.scopes.get(node_h).expect("invalid node to scope mapping");
                let decl = self.symbol_table.search_scope(scope, &identifier).expect("can't find decl");
                locals.push(decl.clone());
                // This doesn't return a node because it doesn't get lowered to anything, oops.
                // Need to make 'Empty' node. 
                self.typed_ast.nodes.insert(TypedASTNode::Empty)
            }
            ASTNode::CompoundStmt { statements, new_scope } => {
                let mut typed_statements = Vec::new();
                for stmt_h in statements {
                    typed_statements.push(self.check_statement(locals, stmt_h));
                }
                let node = TypedASTNode::CompoundStmt { statements: typed_statements };
                self.typed_ast.nodes.insert(node)
            }
            ASTNode::ExpressionStmt { expression } => {
                let (expression, ..) = self.check_expression(expression); 
                let node = TypedASTNode::ExpressionStmt { expression};
                self.typed_ast.nodes.insert(node)
            }
            ASTNode::ReturnStmt { expression } => todo!(),
            ASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            ASTNode::WhileStmt { condition, body } => todo!(),
            ASTNode::IfStmt { condition, if_branch, else_branch } => todo!(),
            ASTNode::BreakStmt => todo!(),
            ASTNode::DeclStmt { declarations } => todo!(),
            ASTNode::InlineAsm { assembly } => todo!(),
            _ => panic!("invalid node for check statement")
        }


    }

    fn check_const_expression(& self, node_h: ASTNodeHandle) -> u32 {
        todo!();
        let node = self.ast.remove(node_h);
        /*
        match node {
            ASTNode::BinaryOp { op, left, right } => {
                match op {
                    // TODO: Support more of these:
                    BinaryOpType::Add
                    BinaryOpType::Div
                    BinaryOpType::Mul
                    BinaryOpType::Sub
                }
            },
            ASTNode::UnaryOp { op, child, order: () } => {
                match op {
                    UnaryOpType::Negate
                }
            }
            _ => Report an error and halt.
        }  */
    }

    fn check_expression(&mut self, node_h: ASTNodeHandle) -> (TypedASTNodeHandle, LR, InternedType) {
        let node = self.ast.remove(node_h);

        match node {
            ASTNode::IntLiteral { value } => {
                let node = TypedASTNode::IntLiteral { value };
                (self.typed_ast.nodes.insert(node), LR::RValue, self.get_int_type())
            }
            ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                // Try array decay.   
                let Some(scope) = self.scopes.get(node_h) else {
                    panic!("node not mapped to scope in typecheck");
                };

                let decl = self.symbol_table.search_tree(scope, &identifier).expect("Unknown symbol");
                let ty = decl.type_info;

                let node = TypedASTNode::SymbolRef { identifier, decl: decl.clone(), ty};
                (self.typed_ast.nodes.insert(node), LR::RValue, self.get_int_type())
            }
            ASTNode::FieldRef { identifier } => todo!(),
            ASTNode::BinaryOp { op, left, right } => {
                match op {
                    BinaryOpType::Assign => {
                        let (left,  _, lty) = self.check_expression(left);
                        let (right, _, rty) = self.try_lvalue_to_rvalue(right);
                        // TODO: Typecheck.
                        let ty = lty;
                        let node = TypedASTNode::BinaryOp { op, left, right, ty };
                        (self.typed_ast.nodes.insert(node), LR::RValue, ty) 
                    }
                    BinaryOpType::ArrayAccess => {
                        todo!()
                    }
                    BinaryOpType::PointerAccess => {
                        todo!()
                    }
                    BinaryOpType::DotAccess => {
                        todo!()
                    }
                    _ => {
                        let (left,  _, lty) = self.try_lvalue_to_rvalue(left);
                        let (right, _, rty) = self.try_lvalue_to_rvalue(right);
                        // This functionality can be moved to a type context.
                        assert_eq!(lty, rty); // TODO: Actually do type coercion.
                        let ty = lty;
                        let node = TypedASTNode::BinaryOp { op, left, right, ty };
                        (self.typed_ast.nodes.insert(node), LR::RValue, ty) 
                    }
                }
            }
            ASTNode::UnaryOp { op, child, order } => {
                match op {
                    UnaryOpType::Address => {
                        /* This converts something to an rvalue.
                         * i.e: int* a = &b; 
                         * a gets b's location (rvalue) as a value (lvalue).
                         */
                        let (child, lr, ty) = self.check_expression(child);
                        // TODO: Better error.
                        if lr != LR::LValue {
                            println!("Error: taking address of rvalue");
                            self.halt = true;
                        }

                        // Address operator simply converts a symbol without 
                        let node = TypedASTNode::UnaryOp { op, child: child, order, ty: ty };
                        // TODO: Make this a pointer type.
                        (self.typed_ast.nodes.insert(node), LR::RValue, ty)
                    }
                    UnaryOpType::Dereference => {
                        // TODO: is this even correct?
                        /* This converts something to an lvalue.
                         * i.e: *a = 10;
                         * a is (lvalue) -> load to rvalue -> deref to lvalue.
                         * child expression must be a pointer. 
                         * and on the right side of an assignment:
                         * a is (lvalue) -> load to rvalue -> deref to lvalue -> load to rvalue.
                         */
                        let (child, lr, ty) = self.try_lvalue_to_rvalue(child);

                        // TODO: Do actual type checking.
                        let node = TypedASTNode::UnaryOp { op, child: child, order, ty: ty };
                        (self.typed_ast.nodes.insert(node), LR::LValue, ty)
                    }
                    _ => {
                        let (child, lr, ty) = self.try_lvalue_to_rvalue(node_h);
                        // TODO: Do actual type checking.
                        let node = TypedASTNode::UnaryOp{ op, child, order, ty};
                        (self.typed_ast.nodes.insert(node), LR::RValue, ty) 
                    }
                }
            }
            ASTNode::Ternary { first, second, third } => {
                todo!();
            }
            _ => panic!("unexpected node encountered during check_expression()")
        }
    }

    fn get_int_type(&self) -> InternedType {
        self.context.get_type(
            &Type {
                declarator: Vec::new(),
                specifier: TypeSpecifier {
                    qualifiers: Qualifiers { cv: None, storage: StorageQual::Auto},
                    base: Some(BaseType::Int),
                }
            }
        )
    }

    fn try_lvalue_to_rvalue(&mut self, node_h: ASTNodeHandle) -> (TypedASTNodeHandle, LR, InternedType) {
        let (node, lr, ty) = self.check_expression(node_h);
        match lr {
            LR::LValue => {
                // TODO: Type here me need to be decayed.
                let cast = TypedASTNode::LvalueToRvalue {child: node, ty};
                (self.typed_ast.nodes.insert(cast), LR::RValue, ty)
            }
            LR::RValue => (node, lr, ty) // May have been moved...
        }
    }

    /* Attempts to decay an expression that is possibly an array to a pointer */
    fn try_array_decay() {
        todo!();
    }

}


/*
impl <'a> Vistior<'a> for TypecheckPass<'a> {
    fn get(&self, node_h: ASTNodeHandle) -> &ASTNode {
        self.ast.get(node_h)
    }

    fn postorder(&mut self, node_h: ASTNodeHandle) -> () {
        let node = self.get(node_h).clone();
        // We only need to check expressions really.
        let value = match node {
            ASTNode::IntLiteral { value } => {
                self.types.insert(node_h, self.get_int_type());
                LR::RValue
            },
            //ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                let Some(scope) = self.scopes.get(node_h) else {
                    panic!()
                };

                let type_info = self.symbol_table.search_tree(scope, &identifier).expect("Unknown symbol").type_info;

                self.types.insert(node_h, type_info);
                self.try_array_decay(node_h) // Need to do this on pointer loads, and lvalue field selections, because these could all be arrays.
                //LR::LValue
            },
            ASTNode::VariableDecl { identifier, initializer, type_info } => {
                // If there is an intiializer and initializer is not an rvalue, then cast it to an rvlaue., then, convert to 
                if let Some(initializer) = initializer {
                    match self.lr.get(initializer).unwrap() {
                        LR::LValue => {self.casts.insert(initializer, TypeCast::LvalueToRvalue);},
                        LR::RValue => {},
                    };
                }
                LR::RValue
            }
            ASTNode::UnaryOp { op, child, order } => {
                match op {
                    UnaryOpType::Address => {
                        // TODO: Make sure child is an Lvalue.
                        if self.lr.get(child) != Some(&LR::LValue) {
                            println!("NO");
                            self.halt = true;
                        }
                        // This converts an L-Value to an R-Value.
                        LR::RValue
                    },
                    UnaryOpType::Dereference => {
                        // This converts R-Value to an L-Value.
                        // Apply 
                        // If child is not an Rvalue, then cast child to Rvalue,
                        match self.lr.get(child).unwrap() {
                            LR::LValue => {self.casts.insert(child, TypeCast::LvalueToRvalue);},
                            LR::RValue => {},
                        };
                        // Then Set output as Lvalue.
                        // Cast child to Rvalue: 
                        // (- load from symbol,
                        //  - )
                        LR::LValue
                    },
                    _ => {
                        match self.lr.get(child).unwrap() {
                            LR::LValue => {self.casts.insert(child, TypeCast::LvalueToRvalue);},
                            LR::RValue => {},
                        };
                        LR::RValue
                    }
                }
            },
            ASTNode::BinaryOp { op, right, left } => {
                match op {
                    BinaryOpType::Assign => {
                        match self.lr.get(right).unwrap() {
                            LR::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        LR::RValue 
                        // Check here that Rleft is Rvalue.
                    },
                    BinaryOpType::ArrayAccess => {
                        // Unsure if this needs casting
                        match self.lr.get(right).unwrap() {
                            LR::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        match self.lr.get(left).unwrap() {
                            LR::LValue => {self.casts.insert(left, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        LR::LValue
                    },
                    BinaryOpType::PointerAccess => {
                        // TODO: Assert that lhs is a lvalue.
                        // Extra dumb dumb stuff.
                        let Some(left_type) = self.types.get(left) else {
                            panic!();
                        };
                        let Some(BaseType::Struct(type_identifier)) = self.context.resolve_type(left_type).specifier.base else {
                            todo!()
                        };
                        let Some(record) = self.symbol_table.get_record(&type_identifier) else {
                            // TODO: Error Out.
                            println!("error: unable to find declaration");  
                            todo!();
                        };
                        let ASTNode::FieldRef { identifier } = self.get(right).clone() else {
                            panic!();
                        }; 

                        /*
                        for field in record.clone().fields.as_slice() {
                            if identifier == field.identifier { 
                                self.uses.insert(right, DeclRef::Field(field.clone()));
                            }
                        };  */
                        LR::LValue
                    }
                    BinaryOpType::DotAccess => {
                        // TODO: Assert that lhs is an rvalue.
                        // Extra dumb dumb stuff.
                        let Some(left_type) = self.types.get(left) else {
                            panic!();
                        };
                        let Some(BaseType::Struct(type_identifier)) = self.context.resolve_type(left_type).specifier.base else {
                            todo!()
                        };
                        let Some(record) = self.symbol_table.get_record(&type_identifier) else {
                            // TODO: Error Out.
                            println!("error: unable to find declaration");  
                            todo!();
                        };
                        let ASTNode::FieldRef { identifier } = self.get(right).clone() else {
                            panic!();
                        }; 
                        
                        /*
                        for field in record.clone().fields.as_slice() {
                            if identifier == field.identifier { 
                                self.uses.insert(right, DeclRef::Field(field.clone()));
                            }
                        };  */
                        LR::LValue

                    }
                    _ => {
                        // Convert lvalues to Rvalues
                        match self.lr.get(right).unwrap() {
                            LR::LValue => {self.casts.insert(right, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        match self.lr.get(left).unwrap() {
                            LR::LValue => {self.casts.insert(left, TypeCast::LvalueToRvalue);},
                            LR::RValue => ()
                        }
                        LR::RValue 
                    }

                    
                }
                // Unless its a array index, then you can get an Lvalue from that
            },
            ASTNode::ReturnStmt { expression } => {
                if let Some(expression) = expression {
                    match self.lr.get(expression).unwrap() {
                        LR::LValue => {self.casts.insert(expression, TypeCast::LvalueToRvalue);},
                        LR::RValue => ()
                    }
                }
                LR::RValue // This shouldn't matter
            }
            _ => LR::RValue,
        };
        self.lr.insert(node_h, value);
    }

    fn halt(& self) -> bool {false}
}

impl <'a, 'ast> TypecheckPass<'ast> {

    pub fn print_casts(&self) -> () {
        for (handle, cast) in &self.casts {
            let node = self.get(handle);
            match node {
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, cast)}
            }
        }
    } 

    pub fn print_lr(&self) -> () {
        // TODO: Make this print and annotate the AST.
        for (handle, lr) in &self.lr {
            let node = self.get(handle);
            match node {
                _ => {println!("{} {}", ASTNodePrintable{node: node.clone(), context: self.context}, lr)}
            }
        }
    }

    fn get_int_type(&self) -> InternedType {
        self.context.get_type(
            &Type {
                declarator: Vec::new(),
                specifier: TypeSpecifier {
                    qualifiers: Qualifiers { cv: None, storage: StorageQual::Auto},
                    base: Some(BaseType::Int),
                }
            }
        )
    }

    fn try_array_decay(& mut self, node_h: ASTNodeHandle) -> LR {
        // Assert that this is a symbol ref
        let ASTNode::SymbolRef { identifier } = self.get(node_h) else {
            panic!()
        };
        let scope = self.scopes.get(node_h).expect("ast node does not have scope");
        let Some(symbol) = self.symbol_table.search_tree(scope, identifier) else {
            return LR::LValue;
        };

        if self.context.resolve_type(&symbol.type_info).is_array() {
            self.casts.insert(node_h, TypeCast::ArrayToPointerDecay);

            let mut r#type = self.context.resolve_type(&symbol.type_info).clone();
            let first = r#type.declarator.first_mut().unwrap();
            *first = DeclaratorPart::PointerDecl(None);

            let r#type = self.context.get_type(&r#type);
            self.types.insert(node_h, r#type);
            LR::RValue

        }
        else {
            LR::LValue
        }        
    }
} */
