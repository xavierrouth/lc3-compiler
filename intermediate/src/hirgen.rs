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
    
    hir: HIR,
}


impl <'a> HIRGen<'a> {
    pub fn new(ast: TypedAST, symbol_table: SymbolTable, context: &'a Context<'a>, 
        locations: HashMap<VarDecl, MemoryLocation>, 
        error_handler: &'a ErrorHandler<'a>) -> HIRGen<'a> {
            HIRGen { symbol_table , ast , context, hir: HIR { main: None, functions: Vec::new(), data: Vec::new() } }
    }

    pub fn run(&mut self) -> HIR {
        let root = self.ast.root.expect("invalid root");
        let root = self.ast.get(root);
        let TypedASTNode::Program { functions, globals } = root else {panic!("root is not of type 'program'")};

        for function in functions {
            let cfg = self.build_function_cfg(*function);
            self.hir.functions.push(Rc::new(RefCell::new(cfg)));
        }

        todo!();
            
    }

    fn emit_expression(&mut self, cfg: &mut CFG, basic_block_h: BasicBlockHandle, node_h: TypedASTNodeHandle) -> InstructionHandle {
        let node = self.ast.get(node_h);
        match node {
            TypedASTNode::Empty => todo!(),
            TypedASTNode::IntLiteral { value } => todo!(),
            TypedASTNode::LvalueToRvalue { child, ty } => {
                // TODO Optimization Potential: reduce LValueToRValue on SymbolRef to load form SymbolRef location.
                // TODO Optimization Potential: reduce LValueToRValue on ArrayAccess to load form ArrayAccess location.
                // TODO Optimization Potential: reduce LValueToRValue on DotAccess on FieldRef to load form FieldRef location.

                // Wow, seems like these could all be handled by similar code?

                // How to pattern match on handles. You can't really huh!.
                let child = self.emit_expression(cfg, basic_block_h, *child);
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
    fn build_function_bb(&mut self, cfg: &mut CFG, basic_block_h: BasicBlockHandle, node_h: TypedASTNodeHandle) -> () {
        let node = self.ast.get(node_h);
        match node {
            // Local Variable.
            TypedASTNode::IntLiteral { value } => {
                cfg.add_inst(basic_block_h, Instruction::Const(*value));
            }
            TypedASTNode::ExpressionStmt { expression } => {self.emit_expression(cfg, basic_block_h, *expression);},
            TypedASTNode::ReturnStmt { expression } => todo!(),
            TypedASTNode::ForStmt { initializer, condition, update, body } => todo!(),
            TypedASTNode::WhileStmt { condition, body } => todo!(),
            TypedASTNode::IfStmt { condition, if_branch, else_branch } => todo!(),
            TypedASTNode::BreakStmt => todo!(),
            TypedASTNode::DeclStmt { declarations } => todo!(),
            TypedASTNode::InlineAsm { assembly } => todo!(),
            _ => todo!()
        }
    }

    /* 
    fn emit_stack_allocation(&mut self, cfg: &mut CFG, node_h: ASTNodeHandle) -> () {
        let = self.ast.get(node_h);
    } */

    // is CFG -> CFG preferred or just &mut CFG -> (). Interesting question
    /* Builds a CFG for a function. */
    fn build_function_cfg(&mut self, node_h: TypedASTNodeHandle) -> CFG {
        let node = self.ast.get(node_h); // ast.remove() instead??

        let TypedASTNode::FunctionDecl { body, identifier, return_type, parameters, locals } = node else {
            panic!("build function called on non-function node");
        };

        let name = identifier;
        //let func_name = self.context.resolve_string(*func_name);
        let mut cfg = CFG::new(*name);

        // Take ownership of params from symbol table probably. We don't want symbol table anymore in HIR.
        for parameter in parameters {
            cfg.add_parameter(parameter.clone());
        }

        // Generate entry basicb block

        todo!();
        for local in locals {
            // Assume non-static and constant size for now

            let size = cfg.get_const(cfg.entry, local.size.try_into().unwrap());
            // Parameter:
            if decl.is_parameter {
                cfg.add_parameter(decl.clone());
            } else { // Local:  
                let alloca = cfg.add_to_entry(Instruction::Allocate(size));
                cfg.add_local(decl.clone(), alloca);
            }

            cfg.add_to_entry(inst)
        }

        match node {
            // These should all be errors lowkey.
            _ => panic!()

        }
        todo!()

    }

}