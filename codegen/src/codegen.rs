use lex_parse::{analysis::SymbolTable, ast::{AST, ASTNodeHandle, ASTNode, BinaryOpType, UnaryOpType}};

use crate::asmprinter::{AsmPrinter, Register, LC3Bundle, LC3Inst, Immediate as Imm, Label};
use lex_parse::strings::{Strings, InternedString};

pub struct Codegen<'a> {
    regfile: [bool; 8],
    global_data_addr: usize,
    user_stack_addr: usize,
    symbol_table: SymbolTable,
    printer: &'a mut AsmPrinter, // 
    ast: &'a AST,
    function: InternedString,
} 

// TODO: Rewrite to use Register data type instead of usize

impl<'a> Codegen<'a> {
    const USED: bool = true;
    const UNUSED: bool = false;
    const R5: Register = Register{value: 5};
    const R6: Register = Register{value: 6};
    const R7: Register = Register{value: 7};

    pub fn new(ast: &'a AST, printer: &'a mut AsmPrinter, symbol_table: SymbolTable) -> Codegen<'a> {
        let mut lock = Strings.lock().unwrap();
        let function = lock.get_or_intern("main");
        Codegen { regfile: [false ; 8], global_data_addr: 0, user_stack_addr: 0, symbol_table: symbol_table, printer: printer, ast: ast, function: function  }
    }
    fn get_empty_reg(&self) -> Register {
        for n in 0..7 {
            if self.regfile[n] == false {
                return Register {value: n}; 
            }
        }
        Register { value: 10 } // TODO: error out here
    }

    fn emit_condition_node(&mut self, node_h: &ASTNodeHandle) {
        let node = self.ast.get_node(node_h);


    }


    pub fn emit_ast_node(&mut self, node_h: &ASTNodeHandle) {
        // If counter, while_counter, for_counter etc..
        let node = self.ast.get_node(node_h);
        match node {
            ASTNode::Program { declarations } => {
                for decl in declarations {
                    self.emit_ast_node(decl);
                }
            }
            // Decls:
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                self.function = *identifier;

                let lock = Strings.lock().unwrap();
                let identifier = lock.resolve(self.function).unwrap();
                    
                self.printer.inst(LC3Bundle::HeaderLabel(Label::Label(identifier.to_string()), None));
                //TODO: Add normal comment type.
                //self.printer.inst(LC3Bundle::HeaderLabel(Label::Label("callee setup:".to_string()), None))
                self.emit_ast_node(body);
                //self.printer.inst(LC3Bundle::HeaderLabel(Label::Label(identifier.to_string()), None))
            },
            ASTNode::ParameterDecl { identifier, r#type } => todo!(),
            ASTNode::VariableDecl { identifier, initializer, r#type } => {

            },
            
            // Statements:
            ASTNode::Ternary { first, second, third } => todo!(),
            ASTNode::CompoundStmt { statements, new_scope } => {
                for stmt in statements {
                    self.emit_ast_node(stmt);
                    // Emit newline
                }
            },
            ASTNode::ExpressionStmt { expression } => todo!(),
            ASTNode::ReturnStmt { expression } => todo!(),
            ASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            ASTNode::WhileStmt { condition, body } => todo!(),
            ASTNode::IfStmt { condition, if_branch, else_branch } => todo!(),
            ASTNode::DeclStmt { declarations } => todo!(),
            ASTNode::InlineAsm { assembly } => todo!(),

            // Expressions:
            ASTNode::IntLiteral { value } => {self.emit_expression_node(node_h); ()},
            ASTNode::FunctionCall { symbol_ref, arguments } => {self.emit_expression_node(node_h); ()},
            ASTNode::SymbolRef { identifier } => {self.emit_expression_node(node_h); ()},
            ASTNode::BinaryOp { op, right, left } => {self.emit_expression_node(node_h); ()},
            ASTNode::UnaryOp { op, child, order } => {self.emit_expression_node(node_h); ()},    
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
                        let reg = self.emit_expression_node(right);
                        self.regfile[reg.value] = Self::USED;
                        
                        // Dereference:
                        if let ASTNode::UnaryOp { op, child: child, order:_ } = left_node {
                            if let UnaryOpType::Dereference = op {
                                let addr = self.emit_expression_node(child);

                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::Str(reg, addr, Imm::Int(0)), Some("dereference pointer".to_string())));
                                    self.regfile[addr.value] = Self::UNUSED;
                            }
                        }
                        else {
                            let entry = self.symbol_table.entries.get(*left).unwrap();

                            let lock = Strings.lock().unwrap();
                            let identifier = lock.resolve(entry.identifier).unwrap();
                            // Static   
                            if entry.type_info.type_specifier.marked_static {
                                
                                let function_name = lock.resolve(self.function).unwrap();
                                

                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::St(reg, Label::Label(format!("{}.{}", function_name, identifier))), Some("assign to static variable".to_string())));
                            } // Normal
                            else {
                                self.printer.inst(LC3Bundle::Instruction(
                                    LC3Inst::Str(reg, Self::R5, Imm::Int(entry.offset)), Some(format!("assign to variable {identifier}"))));
                            }
                        }
                        return reg;
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
                        else {
                            let left = self.emit_expression_node(left);
                            self.regfile[left.value] = Self::USED;
                            let right = self.emit_expression_node(right);
                            
                            self.printer.inst(LC3Bundle::Instruction(
                                LC3Inst::AddReg(left, left, right), None));
                            
                            self.regfile[right.value] = Self::UNUSED;
                            return left;
                        }
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

                    // Conditionals
                    BinaryOpType::LogAnd => todo!(),
                    BinaryOpType::LogOr => todo!(),
                    BinaryOpType::BitAnd => todo!(),
                    BinaryOpType::LessThan => todo!(),
                    BinaryOpType::GreaterThan => todo!(),
                    BinaryOpType::LessThanEqual => todo!(),
                    BinaryOpType::GreaterThanEqual => todo!(),
                    BinaryOpType::NotEqual => todo!(),
                    BinaryOpType::EqualEqual => todo!(),
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

                        // Address of variable is just R5 + offset
                        if entry.type_info.type_specifier.marked_static {
                            let lock = Strings.lock().unwrap();
                            let identifier = lock.resolve(entry.identifier).unwrap();

                            let function_name = lock.resolve(self.function).unwrap();
                            
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::Lea(reg, Label::Label(format!("{}.{}", function_name, identifier)) ), 
                                Some("load address of static variable".to_string())));
                        }
                        else {
                            self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(reg, Self::R5, Imm::Int(entry.offset)), Some("take address of variable".to_string())));
                        }
                        return reg;
                    }
                    UnaryOpType::Dereference => {
                        let reg = self.emit_expression_node(child);
                        self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, reg, Imm::Int(0)), None));
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
                // Push arguments right to left.
                for arg in arguments.into_iter().rev() {
                    let arg_reg = self.emit_expression_node(arg);
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(-1)), None));
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::Str(arg_reg, Self::R6, Imm::Int(0)), Some("push argument to stack.".to_string())));
                    self.regfile[arg_reg.value] = Self::UNUSED;
                    // Emit newline
                }

                // Todo: Support calling arbitrary memory locations as functions.
                let entry = self.symbol_table.entries.get(*symbol_ref).unwrap();

                let lock = Strings.lock().unwrap();
                let identifier = lock.resolve(entry.identifier).unwrap();

                // Emit jump:
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Jsr(Label::Label(identifier.to_string())), Some("call function.".to_string())));
                // Handle return value.
                let ret = self.get_empty_reg();
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(ret, Self::R6, Imm::Int(0)), Some("load return value.".to_string()))); 
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(1)), None));

                let num_args: i32 = arguments.len().try_into().unwrap();
                self.printer.inst(LC3Bundle::Instruction(LC3Inst::AddImm(Self::R6, Self::R6, Imm::Int(num_args)), Some("pop arguments".to_string())));

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

                if entry.type_info.type_specifier.marked_static {
                    let lock = Strings.lock().unwrap();
                    let identifier = lock.resolve(entry.identifier).unwrap();
                    let function_name = lock.resolve(self.function).unwrap();
                    
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ld(reg, Label::Label(format!("{}.{}", function_name, identifier))), Some("load static variable".to_string())));
                }
                else {
                    self.printer.inst(LC3Bundle::Instruction(LC3Inst::Ldr(reg, Self::R5, Imm::Int(entry.offset)), Some("load local variable or parameter".to_string())));
                }
                self.regfile[reg.value] = Self::USED;

                return reg;
            }
            _ => todo!()
        }
        todo!()
    }
}