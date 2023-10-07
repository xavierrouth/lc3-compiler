use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, MemoryLocation, InstructionHandle, self}};

use analysis::{symtab::{SymbolTable, ScopeHandle, VarDecl}, typedast::{TypedAST, TypedASTNode, TypedASTNodeHandle}};
use lex_parse::{ast::{ASTNodeHandle, AST, ASTNode, self}, error::ErrorHandler, context::Context};
use slotmap::{SparseSecondaryMap, SlotMap};

pub struct HIRGen<'a> {
    // External Information
    symbol_table: SymbolTable,

    ast: TypedAST,
    context: &'a Context<'a>,
    
    hir: HIR<'a>,
}


impl <'a> HIRGen<'a> {
    pub fn new(ast: TypedAST, symbol_table: SymbolTable, context: &'a Context<'a>, 
        error_handler: &'a ErrorHandler<'a>) -> HIRGen<'a> {
            HIRGen { symbol_table , ast , context, hir: HIR { main: None, functions: Vec::new(), data: Vec::new(), context } }
    }

    pub fn run(mut self) -> HIR<'a> {
        let root = self.ast.root.expect("invalid root");
        let root = self.ast.remove(root);
        let TypedASTNode::Program { functions, globals } = root else {panic!("root is not of type 'program'")};

        for function in functions {
            let cfg = self.build_function_cfg(function);
            self.hir.functions.push(Rc::new(RefCell::new(cfg)));
        }
        self.hir
    }

    fn emit_expression(&mut self, cfg: &mut CFG<'a>, basic_block_h: BasicBlockHandle, node_h: TypedASTNodeHandle) -> InstructionHandle {
        let node = self.ast.remove(node_h);
        match node {
            TypedASTNode::IntLiteral { value } => {
                cfg.add_inst(basic_block_h, Instruction::Const(value))
            }
            TypedASTNode::LvalueToRvalue { child, ty } => {
                // TODO Optimization Potential: reduce LValueToRValue on SymbolRef to load form SymbolRef location.
                // TODO Optimization Potential: reduce LValueToRValue on ArrayAccess to load form ArrayAccess location.
                // TODO Optimization Potential: reduce LValueToRValue on DotAccess on FieldRef to load form FieldRef location.

                // Wow, seems like these could all be handled by similar code?

                // How to pattern match on handles. You can't really huh!.
                let child = self.emit_expression(cfg, basic_block_h, child);
                let loc = MemoryLocation::Expr(child);
                let load = Instruction::Load(loc);
                let load = cfg.add_inst(basic_block_h, load);
                load
            }
            TypedASTNode::FunctionCall { symbol_ref, arguments, ty } => todo!(),
            TypedASTNode::SymbolRef { identifier, decl, ty } => {
                let loc = cfg.get_location(decl.clone());

                let ty = decl.type_info;
                let ty = self.context.resolve_type(&ty);
          
                let lea_inst = Instruction::Lea(loc.clone());
                let lea_inst = cfg.add_inst(basic_block_h, lea_inst);
                lea_inst
            }
            TypedASTNode::FieldRef { identifier, ty } => todo!(),
            TypedASTNode::BinaryOp { op, left, right, ty } => {
                match op {
                    ast::BinaryOpType::Assign => {
                        let rhs = self.emit_expression(cfg, basic_block_h, right);
                        // Previously we have optimized here, but IG we will just do that later *shrug*.
                        let lhs = self.emit_expression(cfg, basic_block_h, left);
                        // Treat LHS as a memory location.
                        let loc = MemoryLocation::Expr(lhs);
                        let store = cfg.add_inst(basic_block_h, Instruction::Store(loc, rhs));

                        rhs
                    }
                    ast::BinaryOpType::ArrayAccess => {
                        let base = self.emit_expression(cfg, basic_block_h, left);
                        let offset = self.emit_expression(cfg, basic_block_h, right);
                        
                        // Could previously be optimized based on lvalue and rvalue things.
                        let inst = cfg.add_inst(basic_block_h, Instruction::Lea(MemoryLocation::Expr(base)));
                        let inst = cfg.add_inst(basic_block_h, Instruction::BinaryOp(hir::BinaryOpType::Add, inst, offset));
                        inst
                    }
                    _ => {
                        todo!();
                    } 
                }
            }
            TypedASTNode::UnaryOp { op, child, order, ty } => todo!(),
            TypedASTNode::Ternary { first, second, third, ty } => todo!(),
            _ => panic!("Expected expression node.") 
            
        }
    }

    // By nature of using arenas and handles, all references are mutable, should there be separate types?
    fn build_function_bb(&mut self, cfg: &mut CFG<'a>, basic_block_h: &mut BasicBlockHandle, node_h: TypedASTNodeHandle) -> () {
        let node = self.ast.remove(node_h);
        match node {
            // Non-Control changing statements
            TypedASTNode::IntLiteral { value } => {
                cfg.add_inst(*basic_block_h, Instruction::Const(value));
            }
            TypedASTNode::VariableDecl { decl, initializer, type_info } => {
                let size = cfg.get_const(cfg.entry, decl.size.try_into().unwrap());
                // Parameter:
                let alloca = cfg.add_to_entry(Instruction::Allocate(size)); // Add an alloca instruction to entry bb
                cfg.add_local(decl.clone(), alloca); // Add alloca and decl to the locals of the CFG.
                // Unclear if we need separate Alloca instructions.
                // 
                if let Some(initializer) = initializer {
                    let init = self.emit_expression(cfg, *basic_block_h, initializer);
                    let loc = cfg.get_location(decl.clone());
                    cfg.add_inst(*basic_block_h, Instruction::Store(loc.clone(), init));
                }
                
            }
            TypedASTNode::DeclStmt { declarations } => todo!(),
            TypedASTNode::InlineAsm { assembly } => todo!(),
            TypedASTNode::ExpressionStmt { expression } => {
                self.emit_expression(cfg, *basic_block_h, expression);
            },
            TypedASTNode::CompoundStmt { statements } => {
                for stmt in statements {
                    self.build_function_bb(cfg, basic_block_h, stmt);
                }
            }
            // Control changing statements
            TypedASTNode::ReturnStmt { expression } => {
                // TODO: Do we want basic blocks for function teardown and function setup?
            }
            TypedASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            TypedASTNode::WhileStmt { condition, body } => todo!(),
            TypedASTNode::IfStmt { condition, if_branch, else_branch } => {
                // Don't worry about fall through optimizations for now.

                let cond = self.emit_expression(cfg, *basic_block_h, condition);

                let if_name = self.context.get_string("if.branch");
                let mut if_bb: BasicBlockHandle = cfg.new_bb(if_name);

                // TODO: Optimize
                match else_branch {
                    Some(else_branch) => {
                        let else_name = self.context.get_string("else.branch");
                        let mut else_bb = cfg.new_bb(else_name);

                        let end_name = self.context.get_string("end");
                        let end_bb = cfg.new_bb(end_name);

                        let br = Instruction::CondBr(cond, if_bb, else_bb);
                        let br = cfg.add_inst(*basic_block_h, br);

                        self.build_function_bb(cfg, &mut if_bb, if_branch);
                        cfg.add_inst(if_bb, Instruction::Br(end_bb));

                        self.build_function_bb(cfg, &mut else_bb, else_branch);
                        cfg.add_inst(else_bb, Instruction::Br(end_bb));

                        *basic_block_h = end_bb;
                    }
                    None => {
                        let end_name = self.context.get_string("end");
                        let end_bb = cfg.new_bb(end_name);

                        let br = Instruction::CondBr(cond, if_bb, end_bb);
                        let br = cfg.add_inst(*basic_block_h, br);

                        self.build_function_bb(cfg, &mut if_bb, if_branch);
                        cfg.add_inst(if_bb, Instruction::Br(end_bb));

                        *basic_block_h = end_bb;
                    }
                }

            }
            TypedASTNode::BreakStmt => todo!(),
            
            _ => todo!()
        }
    }

    /* 
    fn emit_stack_allocation(&mut self, cfg: &mut CFG, node_h: ASTNodeHandle) -> () {
        let = self.ast.get(node_h);
    } */

    // is CFG -> CFG preferred or just &mut CFG -> (). Interesting question
    /* Builds a CFG for a function. */
    fn build_function_cfg(&mut self, node_h: TypedASTNodeHandle) -> CFG<'a> {
        let node = self.ast.remove(node_h); // ast.remove() instead??

        let TypedASTNode::FunctionDecl { body, identifier, return_type, parameters } = node else {
            panic!("build function called on non-function node");
        };

        let name = identifier;
        //let func_name = self.context.resolve_string(*func_name);
        let mut cfg = CFG::new(name, return_type, self.context);

        // Take ownership of params from symbol table probably. We don't want symbol table anymore in HIR.
        for parameter in parameters {
            cfg.add_parameter(parameter.clone());
        }
        
        let mut entry = cfg.entry;
        // Generate 
        self.build_function_bb(&mut cfg, &mut entry, body);
        cfg

    }

}