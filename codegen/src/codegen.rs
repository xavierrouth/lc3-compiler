use std::fmt::format;
use slotmap::{SparseSecondaryMap};

use ::analysis::{symbol_table::{SymbolTable, DeclarationType}, typecheck::TypeCast};
use lex_parse::{ast::{AST, ASTNodeHandle, ASTNode, BinaryOpType, UnaryOpType}, types::StorageQual};
use analysis::{analysis};
use crate::asmprinter::{AsmPrinter, Register, LC3Bundle, LC3Bundle::*, LC3Inst, Immediate as Imm, Label, LC3Directive};
use lex_parse::context::{Context, InternedType, InternedString};


pub struct Codegen<'a> {
    // State
    regfile: [bool; 8],
    global_data_addr: usize,
    user_stack_addr: usize,
    scope: InternedString,

    // External Information
    symbol_table: SymbolTable,


    printer: &'a mut AsmPrinter, // 
    ast: &'a AST,
    context: &'a Context<'a>,
    casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,

    // Random trash:
    if_counter: usize,
    while_counter: usize,
    for_counter: usize,
} 

// TODO: Rewrite to use Register data type instead of usize

impl<'a> Codegen<'a> {
    const USED: bool = true;
    const UNUSED: bool = false;
    const R5: Register = Register{value: 5};
    const R6: Register = Register{value: 6};
    const R7: Register = Register{value: 7};

    pub fn new(ast: &'a AST, printer: &'a mut AsmPrinter, context: &'a Context<'a>, symbol_table: SymbolTable, casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>) -> Codegen<'a> {
        Codegen { regfile: [false ; 8], global_data_addr: 0, user_stack_addr: 0, context, symbol_table, printer, ast, casts, scope: context.get_string("global"),
        if_counter: 0, while_counter: 0, for_counter: 0}
    }

    fn get_empty_reg(&self) -> Register {
        // TODO: Flag  
        for n in 0..4 {
            if self.regfile[n] == false {
                return Register {value: n}; 
            }
        }
        println!("error: cannot allocate registers effectively, please move complex expressions to separate statements.");
        panic!(); // TODO: Surpress panic.
    }

    fn reset_regfile(&mut self) -> () {
        for n in 0..4 {
            self.regfile[n] = false;
        }
    }

    fn emit_condition_node(&mut self, node_h: &ASTNodeHandle) -> Register {
        let node = self.ast.get_node(node_h);

        let (op, left, right) = match node {
            ASTNode::BinaryOp{op, left, right} => (op, left, right),
            _ => panic!()
        };

        let right = self.emit_expression_node(right);
        self.regfile[right.value] = Codegen::USED;
        let left = self.emit_expression_node(left);

        let ret = left;

        match op {
            BinaryOpType::LessThan => {
                self.printer.inst(Instruction(LC3Inst::Not(left, left), Some("evaluate '<'".to_string())));
                self.printer.inst(Instruction(LC3Inst::AddImm(left, left, Imm::Int(1)), None));
                self.printer.inst(Instruction(LC3Inst::AddReg(ret, left, right), None));
            },
            BinaryOpType::GreaterThan => {
                self.printer.inst(Instruction(LC3Inst::Not(right, right), Some("evaluate '>'".to_string())));
                self.printer.inst(Instruction(LC3Inst::AddImm(right, right, Imm::Int(1)), None));
                self.printer.inst(Instruction(LC3Inst::AddReg(ret, left, right), None));
            },
            _ => panic!()
        }

        return ret;
    }


    pub fn emit_ast_node(&mut self, node_h: &ASTNodeHandle) {
        // If counter, while_counter, for_counter etc..
        let node = self.ast.get_node(node_h);
        match node {
            ASTNode::Program { declarations } => {

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ld(Self::R6, Label::Label("USER_STACK".to_string())), None));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R5, Self::R6, Imm::Int(-1)), None));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Jsr(Label::Label("main".to_string())), None));

                self.printer.inst(LC3Bundle::Newline);
                for decl in declarations {
                    self.emit_ast_node(decl);
                }
                //self.printer.inst(LC3Bundle::Instruction(LC3Inst::Halt, None));
                //self.printer.data(LC3Bundle::Directive(Some(Label::Label("USER_STACK".to_string)), .LC3Directive::Fill(()), ()))
            }
            // Decls:
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                self.printer.inst(Newline);
                self.scope = *identifier;

                let function = self.context.resolve_string(self.scope);
                let identifier = self.context.resolve_string(*identifier);

                if function == "main" {
                    self.printer.inst(LC3Bundle::HeaderLabel(Label::Label(identifier.clone()), None));
                    self.emit_ast_node(body);
                    return;
                }
                
                self.printer.inst(LC3Bundle::HeaderLabel(Label::Label(identifier.clone()), None));
                // TODO: Support 'builder' syntax for these instructions. The entire first 20 chars really should not be necessary. 
                self.printer.inst(LC3Bundle::SectionComment("callee setup:".to_string()));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), Some("allocate spot for return value".to_string())));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), None));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(Self::R7, Self::R6, Imm::Int(0)), Some("push R7 (return address)".to_string())));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), None));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(Self::R5, Self::R6, Imm::Int(0)), Some("push R5 (caller frame pointer)".to_string())));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R5, Self::R6, Imm::Int(-1)), Some("set frame pointer".to_string())));

                self.printer.inst(LC3Bundle::Newline);
                self.printer.inst(LC3Bundle::SectionComment("function body:".to_string()));
                self.emit_ast_node(body);

                let teardown_label = format!("{identifier}.teardown").to_string();

                self.printer.inst(LC3Bundle::HeaderLabel(Label::Label(teardown_label), None));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R5, Imm::Int(1)), Some("pop local variables".to_string())));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(Self::R5, Self::R6, Imm::Int(0)), Some("pop frame pointer".to_string())));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(1)), None));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(Self::R7, Self::R6, Imm::Int(0)), Some("pop return address".to_string())));
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(1)), None));

                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ret, None));
                self.printer.inst(LC3Bundle::SectionComment("end function.".to_string()));
                self.printer.inst(Newline);
            },
            ASTNode::ParameterDecl { identifier, type_info } => {},
            ASTNode::VariableDecl { identifier, initializer, type_info } => {

                let identifier = self.context.resolve_string(*identifier);
                

                let entry = self.symbol_table.entries.get(*node_h).unwrap();
                // Global Variable
                if entry.is_global {
                    let value = match initializer {
                        Some(initializer) => {
                            // Need to do constant evaluation.
                            if let ASTNode::IntLiteral { value } = self.ast.get_node(initializer) {
                                *value
                            }
                            else {0}
                        }
                        None => 0
                    };
                    
                    self.printer.data(LC3Bundle::Directive(Some(Label::Label(identifier)), LC3Directive::Fill(value), None));
                    return;
                }

                let function = self.context.resolve_string(self.scope);
                // Static Local Variable
                if self.context.resolve_type(entry.type_info).specifier.qualifiers.storage == StorageQual::Static {
                    let full_identifier = format!("{function}.{identifier}");   
                    // TODO: Constant evaluation of expressions
                    let value = match initializer {
                        Some(initializer) => {
                            // Need to do constant evaluation.
                            if let ASTNode::IntLiteral { value } = self.ast.get_node(initializer) {
                                *value
                            }
                            else {0}
                        }
                        None => 0
                    };
                    self.printer.data(LC3Bundle::Directive(Some(Label::Label(full_identifier)), LC3Directive::Fill(value), None));
                }

                // Nonstatic local variable
                else {
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1 * entry.size as i32)), Some(format!("allocate space for '{identifier}'"))));
                    match initializer {
                        Some(initializer) => {
                            // Need to do constant evaluation.
                            let reg = self.emit_expression_node(initializer);
                            let entry = self.symbol_table.entries.get(*node_h).unwrap();
                            self.regfile[reg.value] = Self::UNUSED;

                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(reg, Self::R5, Imm::Int(entry.offset)), Some(format!("initialize '{identifier}'"))));
                        }
                        None => ()
                    }
                }
            },
            
            // Statements:
            ASTNode::Ternary { first, second, third } => todo!(),
            ASTNode::CompoundStmt { statements, new_scope } => {
                for stmt in statements {
                    self.emit_ast_node(stmt);
                    self.printer.inst(LC3Bundle::Newline);
                    self.reset_regfile();
                }
            },
            ASTNode::ExpressionStmt { expression } => todo!(),
            ASTNode::ReturnStmt { expression } => {
                let function = self.context.resolve_string(self.scope);

                if function == "main" {
                    match expression {
                        Some(expression) => {
                            let reg = self.emit_expression_node(expression);
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Sti(reg, Label::Label("RETURN_SLOT".to_string())), Some("write return value from main".to_string())));
                            self.printer.inst(Instruction(LC3Inst::Halt, None));
                        },
                        None =>  ()
                    }
                }
                else {
                    match expression {
                        Some(expression) => {
                            let reg = self.emit_expression_node(expression);
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(reg, Self::R5, Imm::Int(3)), Some("write return value, always R5 + 3".to_string())));
                        },
                        None =>  ()
                    }
    
                    let teardown_label = format!("{function}.teardown").to_string();
    
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::Br(true, true, true, Label::Label(teardown_label)), None));
                }   
                
                // Maybe we do "RETURN slot"
                    
            }

            /**
            ASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            ASTNode::WhileStmt { condition, body } => todo!(),
            
            ASTNode::DeclStmt { declarations } => todo!(),
            ASTNode::InlineAsm { assembly } => todo!(),
             */
            ASTNode::IfStmt { condition, if_branch, else_branch } => {
                // TOOD: If this is a simple condition, then we don't need to load the condition into NZP.
                let condition = self.emit_expression_node(condition);
                self.printer.inst(Newline);
                self.printer.inst(Instruction(LC3Inst::AndReg(condition, condition, condition), Some("load condition into NZP".to_string())));

                let func_name = self.context.resolve_string(self.scope);

                let name = format!("{func_name}.if.{}", self.if_counter);
                let label_header = Label::Label(format!("{name}"));
                let label_end = Label::Label(format!("{name}.end"));
                let label_else: Label = Label::Label(format!("{name}.else"));

                match else_branch {
                    Some(else_branch) => {
                        self.printer.inst(Instruction(LC3Inst::Br(true, true, false, label_else.clone()), Some("if false, jump to else block".to_string())));
                        self.printer.inst(Newline);
                        self.emit_ast_node(if_branch);

                        self.printer.inst(Instruction(LC3Inst::Br(true, true, true, label_end.clone()), None));
                        self.printer.inst(HeaderLabel(label_else, None));
                        self.printer.inst(Newline);

                        self.emit_ast_node(else_branch);
                        self.printer.inst(HeaderLabel(label_end, None));
                    }
                    None => {
                        self.printer.inst(Instruction(LC3Inst::Br(true, true, false, label_end.clone()), Some("if false, jump over if statement".to_string())));
                        self.printer.inst(Newline);
                        self.emit_ast_node(if_branch);
                        self.printer.inst(HeaderLabel(label_end, None));
                    }
                }

                self.if_counter += 1;
            }

            // Expressions:
            ASTNode::IntLiteral { value } => {self.emit_expression_node(node_h); ()},
            ASTNode::FunctionCall { symbol_ref, arguments } => {self.emit_expression_node(node_h); ()},
            ASTNode::SymbolRef { identifier } => {self.emit_expression_node(node_h); ()},
            ASTNode::BinaryOp { op, right, left } => {self.emit_expression_node(node_h); ()},
            ASTNode::UnaryOp { op, child, order } => {self.emit_expression_node(node_h); ()},
            _ => {
                println!("Unimplemeneted.")
            }    
        }
        
    }

    fn emit_expression_node(&mut self, node_h: &ASTNodeHandle) -> Register {
        
        let node = self.ast.get_node(node_h);

        match node {
            ASTNode::BinaryOp { op, right, left } => {
                let left_node = self.ast.get_node(left);
                let right_node = self.ast.get_node(right);

                match op {
                    // Lots of annoying stuff here, be prepared
                    BinaryOpType::Assign => {

                        // Handle RHS first:
                        let rhs = self.emit_expression_node(right);
                        self.regfile[rhs.value] = Self::USED;

                        // Handle LHS now
                        
                        // OPTIMIZATION:
                        if let ASTNode::SymbolRef { identifier } = left_node {
                            let entry = self.symbol_table.entries.get(*left).unwrap();
                            
                            let identifier = self.context.resolve_string(entry.identifier);

                            // Static   
                            if self.context.resolve_type(entry.type_info).specifier.qualifiers.storage == StorageQual::Static {
                                
                                let function = self.context.resolve_string(self.scope);
                                
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::St(rhs, Label::Label(format!("{function}.{identifier}"))), Some("assign to static variable".to_string())));
                            } // Normal
                            else {
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::Str(rhs, Self::R5, Imm::Int(entry.offset)), Some(format!("assign to variable {identifier}"))));
                            }
                        }
                        else {
                            // In general, lhs should evaluate to an adrress.
                            let lhs = self.emit_expression_node(left);
                            // Treat LHS as address
                            self.printer.inst(Instruction(
                                LC3Inst::Str(rhs, lhs, Imm::Int(0)), None)); // Store RHS into LHS as address
                        }
                        
                        rhs
                        
                        // 

                        // Load 
                        /*
                        let reg = self.emit_expression_node(right);
                        self.regfile[reg.value] = Self::USED;
                        
                        
                        // If the left is a simple symbol,
                        if let ASTNode::SymbolRef { identifier } = left_node {
                            let entry = self.symbol_table.entries.get(*left).unwrap();
                            
                            let identifier = self.context.resolve_string(entry.identifier);

                            // Static   
                            if self.context.resolve_type(entry.type_info).specifier.qualifiers.storage == StorageQual::Static {
                                
                                let function = self.context.resolve_string(self.scope);
                                
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::St(reg, Label::Label(format!("{function}.{identifier}"))), Some("assign to static variable".to_string())));
                            } // Normal
                            else {
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::Str(reg, Self::R5, Imm::Int(entry.offset)), Some(format!("assign to variable {identifier}"))));
                            }
                        }
                        else if let ASTNode::UnaryOp { op, child: child, order:_ } = left_node {
                            if let UnaryOpType::Dereference = op {
                                let addr = self.emit_expression_node(child);

                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::Str(reg, addr, Imm::Int(0)), Some("dereference pointer".to_string())));
                                    self.regfile[addr.value] = Self::UNUSED;
                                
                                return reg;
                            }
                        }  */
                        /*                        // Anything else, treat left side as a memory address.
                        let addr = self.emit_expression_node(left);
                        return reg;  */

                    }
                    BinaryOpType::Add => {
                        if let ASTNode::IntLiteral { value } = left_node {
                            if *value <= 15 {
                                let reg = self.emit_expression_node(right);
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::AddImm(reg, reg, Imm::Int(*value)), None));
                                self.regfile[reg.value] = Self::USED;
                                return reg;
                            }
                        }
                        else if let ASTNode::IntLiteral { value } = right_node {
                            if *value <= 15 {
                                let reg = self.emit_expression_node(left);
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::AddImm(reg, reg, Imm::Int(*value)), None));
                                self.regfile[reg.value] = Self::USED;
                                return reg;
                            } 
                        }
                        
                        let left = self.emit_expression_node(left);
                        self.regfile[left.value] = Self::USED;
                        let right = self.emit_expression_node(right);
                        
                        self.printer.inst(LC3Bundle::Instruction(
                            LC3Inst::AddReg(left, left, right), None));
                        
                        self.regfile[right.value] = Self::UNUSED;
                        return left;
                        
                    }
                    BinaryOpType::Sub => {
                        if let ASTNode::IntLiteral { value } = right_node {
                            let reg = self.emit_expression_node(left);
                            self.regfile[reg.value] = Self::USED;

                            self.printer.inst(LC3Bundle::Instruction(
                                LC3Inst::AddImm(reg, reg, Imm::Int(-1 * value)), None));

                            return reg;
                        }
                        else {
                            let left = self.emit_expression_node(left);
                            self.regfile[left.value] = Self::USED;
                            let right = self.emit_expression_node(right);
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Not(right, right), None));
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(right, right, Imm::Int(1)), None));
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddReg(left, right, left), None));
                            self.regfile[right.value] = Self::UNUSED;

                            return left;
                        }
                    }
                    // Subroutines
                    BinaryOpType::Mul => todo!(),
                    BinaryOpType::Div => todo!(),
                    BinaryOpType::Mod => todo!(),
                    BinaryOpType::BitAnd => todo!(),
                    BinaryOpType::BitOr => todo!(),

                    // Conditionals
                    BinaryOpType::LogAnd |
                    BinaryOpType::LogOr |
                    BinaryOpType::LessThan |
                    BinaryOpType::GreaterThan |
                    BinaryOpType::LessThanEqual |
                    BinaryOpType::GreaterThanEqual |
                    BinaryOpType::NotEqual |
                    BinaryOpType::EqualEqual => return self.emit_condition_node(node_h),

                    // Pointer Access
                    BinaryOpType::ArrayAccess => {
                        let base = self.emit_expression_node(left);
                        self.regfile[base.value] = Self::USED;
                        let offset = self.emit_expression_node(right);
                        self.regfile[offset.value] = Self::USED;

                        let reg = self.get_empty_reg();

                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddReg(reg, base, offset), Some("calculate index into array".to_string())));

                        // Only load if this needs to be an Rvalue, else we just want the address.
                        if self.casts.get(*node_h) == Some(&TypeCast::LvalueToRvalue) {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, reg, Imm::Int(0)), Some("load element from array".to_string())));
                        }

                        self.regfile[base.value] = Self::UNUSED;
                        self.regfile[offset.value] = Self::UNUSED;
                        self.regfile[reg.value] = Self::USED;
                        return reg;

                    }
                    BinaryOpType::DotAccess => todo!(),
                    BinaryOpType::PointerAccess => todo!(),
                    
                }
            },
            ASTNode::UnaryOp { op, child, order } => {
                match op {
                    UnaryOpType::Increment => todo!(),
                    UnaryOpType::Decrement => todo!(),
                    UnaryOpType::Address => {
                        let child_node = self.ast.get_node(child);
                        // Assert that it is an lvalue. (semant checks this)
                        let entry = self.symbol_table.entries.get(*child).unwrap();
                        let reg = self.get_empty_reg();
                        let identifier = self.context.resolve_string(entry.identifier);

                        // Address of variable is just R5 + offset
                        if self.context.resolve_type(entry.type_info).specifier.qualifiers.storage == StorageQual::Static {

                            
                            let function_name = self.context.resolve_string(self.scope);
                            
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Lea(reg, Label::Label(format!("{}.{}", function_name, identifier)) ), 
                                Some("load address of static variable".to_string())));
                        }
                        else {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(reg, Self::R5, Imm::Int(entry.offset)), 
                                Some(format!("take address of '{identifier}'"))));
                        }
                        return reg;
                    }
                    UnaryOpType::Dereference => { // Dereference alwqasy results in an L-value.
                        // Treat whatever is being dereferenced as a memory address, and just load from the address.
                        
                        let reg = self.emit_expression_node(child);
                       
                        // TBH i don't know why this works.
                        if self.casts.get(*node_h) == Some(&TypeCast::LvalueToRvalue) {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, reg, Imm::Int(0)), Some("dereference".to_string())));
                        }
                        //
                        return reg;
                    }
                    UnaryOpType::Negate => {
                        let reg: Register = self.emit_expression_node(child);
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::Not(reg, reg), None));
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(reg, reg, Imm::Int(1)), None));
                        return reg;
                    }
                    UnaryOpType::Positive => {
                        return self.emit_expression_node(child);
                    },
                    UnaryOpType::LogNot => todo!(),
                    UnaryOpType::BinNot => todo!(),
                }
            },
            ASTNode::FunctionCall { symbol_ref, arguments } => {

                // TODO:
                // Push in-use regs

                let mut caller_saved: Vec<Register> = Vec::new();

                for n in 0..7 {
                    if self.regfile[n] == true {
                        let reg = Register {value: n};
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), None));
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(reg, Self::R6, Imm::Int(0)), Some(format!("caller save {reg}"))));
                        caller_saved.push(reg);
                        self.printer.inst(Newline);
                    }
                }


                // Push arguments right to left.
                for arg in arguments.into_iter().rev() {
                    let arg_reg = self.emit_expression_node(arg);
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), None));
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(arg_reg, Self::R6, Imm::Int(0)), Some("push argument to stack.".to_string())));
                    self.printer.inst(Newline);
                    self.regfile[arg_reg.value] = Self::UNUSED;
                    // Emit newline
                }

                // Todo: Support calling arbitrary memory locations as functions.
                let entry = self.symbol_table.entries.get(*symbol_ref).unwrap();

                let identifier = self.context.resolve_string(entry.identifier);
                self.printer.inst(Newline);
                // Emit jump:
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Jsr(Label::Label(identifier.to_string())), Some("call function.".to_string())));
                self.printer.inst(Newline);
                // Handle return value.
                let ret = self.get_empty_reg();
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(ret, Self::R6, Imm::Int(0)), Some("load return value.".to_string()))); 
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(1)), None));

                let num_args: i32 = arguments.len().try_into().unwrap();
                if num_args != 0 {
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(num_args)), Some("pop arguments".to_string())));
                }
                
                // Restore regs
                for reg in caller_saved {
                    self.printer.inst(Newline);
                    self.printer.inst(Instruction(LC3Inst::Ldr(reg, Self::R6, Imm::Int(0)), Some(format!("caller restore {reg}")))); 
                    self.printer.inst(Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(1)), None));
                    self.printer.inst(Newline);
                }
                self.printer.inst(Newline);
                return ret;
            },
            ASTNode::IntLiteral { value } => {
                let reg = self.get_empty_reg();
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AndImm(reg, reg, Imm::Int(0)), None));
                if *value != 0 {
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(reg, reg, Imm::Int(*value)), None));
                }
                return reg;
            }
            ASTNode::SymbolRef { identifier } => {
                let entry = self.symbol_table.entries.get(*node_h).unwrap();
                let reg = self.get_empty_reg();

                if self.casts.get(*node_h) == Some(&TypeCast::LvalueToRvalue) {
                    let identifier = self.context.resolve_string(entry.identifier);
                    
                    /* */
                    // Not supporting static arrays for now.
                    //if entry.type_info.is_array() {
                    //    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(reg, Self::R5, Imm::Int(entry.offset)), Some("calculate base of array".to_string())));
                    //}
                    
                    if self.context.resolve_type(entry.type_info).specifier.qualifiers.storage == StorageQual::Static {
                        
                        let function_name = self.context.resolve_string(self.scope);
                        
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ld(reg, Label::Label(format!("{}.{}", function_name, identifier))), Some("load static variable".to_string())));
                    }
                    else {
                        if entry.kind == DeclarationType::Var {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, Self::R5, Imm::Int(entry.offset)), Some(
                                format!("load local variable '{identifier}'"))));
                        }
                        else {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, Self::R5, Imm::Int(entry.offset)), Some(
                                format!("load parameter '{identifier}'"))));
                        }
                    }
                }
                else if self.casts.get(*node_h) == Some(&TypeCast::ArrayToPointerDecay) {
                    let identifier = self.context.resolve_string(entry.identifier);
                    // this is an lvalue, just generate the address
                    self.printer.inst(Instruction(LC3Inst::AddImm(reg, Self::R5, Imm::Int(entry.offset)), Some(
                        format!("load base of array access for '{identifier}'"))));
                }
                else {
                    // This is an Lvalue, just generate the Address
                    self.printer.inst(Instruction(LC3Inst::AddImm(reg, Self::R5, Imm::Int(entry.offset)), None));

                }
                self.regfile[reg.value] = Self::USED;
                    return reg;
                
            }
            _ => todo!()
        }
    }
}