use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, MemoryLocation, InstructionHandle, self}};

use analysis::{typecheck::TypeCast, symtab::{SymbolTable, ScopeHandle, VarDecl}, typedast::{TypedAST, TypedASTNode}};
use lex_parse::{ast::{ASTNodeHandle, AST, ASTNode, self}, error::ErrorHandler, context::Context};
use slotmap::{SparseSecondaryMap, SlotMap};

pub struct HIRGen<'a> {
    // External Information
    symbol_table: SymbolTable,
    casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>,
    scopes: SparseSecondaryMap<ASTNodeHandle, ScopeHandle>,

    ast: TypedAST,
    context: &'a Context<'a>,
    
    hir: HIR,
}


impl <'a> HIRGen<'a> {
    pub fn new(ast: TypedAST, symbol_table: SymbolTable, context: &'a Context<'a>, 
        casts: SparseSecondaryMap<ASTNodeHandle, TypeCast>, locations: HashMap<VarDecl, MemoryLocation>, scopes: SparseSecondaryMap<ASTNodeHandle, ScopeHandle>,
        error_handler: &'a ErrorHandler<'a>) -> HIRGen<'a> {
            HIRGen { symbol_table , ast , context , casts, hir: HIR { main: None, functions: Vec::new(), data: Vec::new() }, scopes }
    }

    pub fn run(&mut self) -> HIR {
        let root = self.ast.root.expect("invalid root");
        let root = self.ast.get(root);
        let TypedASTNode::Program { functions, globals } = root else {panic!("root is not of type 'program'")};

        for decl in declarations {
            match self.ast.get(*decl) {
                ASTNode::FunctionDecl { body, parameters, identifier, return_type } => {
                    let cfg = self.build_function_cfg(*decl);
                    self.hir.functions.push(Rc::new(RefCell::new(cfg)));
                },
                ASTNode::VariableDecl { identifier, initializer,  type_info,  } => {

                    self.hir.data.push()
                }
                _ => (),
            }
        }
        todo!()
    }

    fn emit_expression(&mut self, cfg: &mut CFG, basic_block_h: BasicBlockHandle, node_h: ASTNodeHandle) -> InstructionHandle {
        let node = self.ast.get(node_h);
        match node {
            ASTNode::IntLiteral { value } => todo!(),
            ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => {
                let scope = self.scopes.get(node_h).expect("invalid node handle");
                let decl = self.symbol_table.search_tree(scope, identifier).expect("invalid var decl in symbol ref");

                let loc = cfg.get_location(decl.clone());

                let ty = decl.type_info;
                let ty = self.context.resolve_type(&ty);

                if self.casts.get(node_h) == Some(&TypeCast::LvalueToRvalue) {
                    let load_inst = Instruction::Load(loc.clone());
                    let load_inst = cfg.add_inst(basic_block_h, load_inst);
                    load_inst
                }
                else {
                    let lea_inst = Instruction::Lea(loc.clone());
                    let lea_inst = cfg.add_inst(basic_block_h, lea_inst);
                    lea_inst
                }
            }
            ASTNode::FieldRef { identifier } => todo!(),
            ASTNode::BinaryOp { op, left, right } => {
                match op {
                    ast::BinaryOpType::Assign => {
                        let rhs = self.emit_expression(cfg, basic_block_h, *right);
                        // Previously we have optimized here, but IG we will just do that later *shrug*.
                        let lhs = self.emit_expression(cfg, basic_block_h, *left);
                        // Treat LHS as a memory location.
                        let loc = MemoryLocation::Expr(lhs);
                        let store = Instruction::Store(loc, rhs);
                        rhs
                    }
                    ast::BinaryOpType::ArrayAccess => {
                        let base = self.emit_expression(cfg, basic_block_h, *left);
                        let offset = self.emit_expression(cfg, basic_block_h, *right);

                        if self.casts.get(node_h) == Some(&TypeCast::LvalueToRvalue) {
                            let load_inst = Instruction::LoadOffset(MemoryLocation::Expr(base), offset);
                            let load_inst = cfg.add_inst(basic_block_h, load_inst);
                            load_inst
                        }
                        else {
                            let inst = Instruction::Lea(MemoryLocation::Expr(base));
                            let inst = cfg.add_inst(basic_block_h, inst);
                            let inst = Instruction::BinaryOp(hir::BinaryOpType::Add, inst, offset);
                            let inst = cfg.add_inst(basic_block_h, inst);
                            inst
                        }
                    }
                    _ => {
                        todo!();
                    } 
                }
            }
            ASTNode::UnaryOp { op, child, order } => todo!(),
            ASTNode::Ternary { first, second, third } => todo!(),
            _ => panic!("Expected expression.")
        }
    }

    // By nature of using arenas and handles, all references are mutable, should there be separate types?
    fn build_function_bb(&mut self, cfg: &mut CFG, basic_block_h: BasicBlockHandle, node_h: ASTNodeHandle) -> () {
        let node = self.ast.get(node_h);
        match node {
            // Local Variable.
            ASTNode::VariableDecl { identifier, initializer, type_info } => {
                // Assume non-static and constant size for now
                let scope = self.scopes.get(node_h).expect("invalid node handle");
                let decl = self.symbol_table.search_tree(scope, identifier).expect("invalid var decl");

                let size = cfg.get_const(basic_block_h, decl.size.try_into().unwrap());
                // Parameter:
                if decl.is_parameter {
                    cfg.add_parameter(decl.clone());
                } else { // Local:  
                    let alloca = cfg.add_to_entry(Instruction::Allocate(size));
                    cfg.add_local(decl.clone(), alloca);
                }
            },
            ASTNode::IntLiteral { value } => {
                cfg.add_inst(basic_block_h, Instruction::Const(*value));
            }
            ASTNode::ExpressionStmt { expression } => {self.emit_expression(cfg, basic_block_h, *expression);},
            ASTNode::ReturnStmt { expression } => todo!(),
            ASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            ASTNode::WhileStmt { condition, body } => todo!(),
            ASTNode::IfStmt { condition, if_branch, else_branch } => todo!(),
            ASTNode::BreakStmt => todo!(),
            ASTNode::DeclStmt { declarations } => todo!(),
            ASTNode::InlineAsm { assembly } => todo!(),
            _ => todo!()
        }
    }

    /* 
    fn emit_stack_allocation(&mut self, cfg: &mut CFG, node_h: ASTNodeHandle) -> () {
        let = self.ast.get(node_h);
    } */

    // is CFG -> CFG preferred or just &mut CFG -> (). Interesting question
    /* Builds a CFG for a function. */
    fn build_function_cfg(&mut self, node_h: ASTNodeHandle) -> CFG {
        let node = self.ast.get(node_h);
        let name = match node {
            ASTNode::FunctionDecl { identifier, .. } => {identifier}
            _ => todo!()
        };
        //let func_name = self.context.resolve_string(*func_name);
        let mut cfg = CFG::new(*name);

        let ASTNode::FunctionDecl { body, parameters, identifier, return_type } = node else {panic!()};

        for parameter in parameters {
            let ASTNode::ParameterDecl { identifier, type_info } = self.ast.get(*parameter) else {panic!()};

            let scope = self.scopes.get(*parameter).expect("parameter with no scope");
            let decl = self.symbol_table.search_tree(scope, identifier).expect("invalid identifier or scope for parameter");

            cfg.add_parameter(decl.clone());
        }

        match node {
            // These should all be errors lowkey.
            ASTNode::Program { declarations } => todo!(),
            ASTNode::FunctionDecl { body, parameters, identifier, return_type } => todo!(),
            ASTNode::RecordDecl { identifier, record_type, fields } => todo!(),
            ASTNode::FieldDecl { identifier, type_info } => todo!(),
            ASTNode::ParameterDecl { identifier, type_info } => todo!(),

            ASTNode::VariableDecl { identifier, initializer, type_info } => {
                return cfg;
            }
            
            ASTNode::IntLiteral { value } => todo!(),
            ASTNode::FunctionCall { symbol_ref, arguments } => todo!(),
            ASTNode::SymbolRef { identifier } => todo!(),
            ASTNode::FieldRef { identifier } => todo!(),
            ASTNode::BinaryOp { op, left, right } => todo!(),
            ASTNode::UnaryOp { op, child, order } => todo!(),
            ASTNode::Ternary { first, second, third } => todo!(),
            ASTNode::CompoundStmt { statements, new_scope } => todo!(),
            ASTNode::ExpressionStmt { expression } => todo!(),
            ASTNode::ReturnStmt { expression } => todo!(),
            ASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            ASTNode::WhileStmt { condition, body } => todo!(),
            ASTNode::IfStmt { condition, if_branch, else_branch } => todo!(),
            ASTNode::BreakStmt => todo!(),
            ASTNode::DeclStmt { declarations } => todo!(),
            ASTNode::InlineAsm { assembly } => todo!(),

        }
        todo!()

    }

}