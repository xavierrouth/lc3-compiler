use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, InstructionHandle, self}, lir::{LIR, Block, Inst, Register, RegisterOrImmediate, Immediate, InstHandle, Function, Label}};

use analysis::{symtab::{SymbolTable, ScopeHandle, VarDecl}, typedast::{TypedAST, TypedASTNode, TypedASTNodeHandle}};
use lex_parse::{ast::{ASTNodeHandle, AST, ASTNode, self}, error::ErrorHandler, context::Context};
use slotmap::{SparseSecondaryMap, SlotMap};

/* Lowering pass from HIR -> LIR, does not generate a completely valid LC3 instructons, needs to be register allocated, 
*  and validated (immediate sizes, imm, imm operands, etc.) */
/* Bascially LIR is invalid LC3, they mostly are the same besides that. */
pub struct LIRGen<'a> {
    // External Information
    hir: HIR<'a>,
    context: &'a Context<'a>,

    hreg_to_lreg: HashMap<InstructionHandle, InstHandle>,
    lir: LIR<'a>,
}


impl <'a> LIRGen<'a> {
    pub fn new(hir: HIR<'a>, context: &'a Context<'a>) -> LIRGen<'a> {
        LIRGen {hir, context, 
            lir: LIR
            { 
                instruction_arena: SlotMap::with_key(), 
                data: Vec::new(), functions: Vec::new(), context, 
            },
            hreg_to_lreg: HashMap::new(), 
        }
    }

    /* Calculates stack frame and such, handle allocas here. JK i don't feel like it lOL! */
    pub fn prepare_cfg(&mut self, cfg: &mut CFG<'a>) -> () {
        /* We could remove all allocas here, but that is lamerton! */
        cfg.instruction_arena
    }

    pub fn run(mut self) -> LIR<'a> {
        /* Emit setup */
        for function in self.hir.functions.clone() { // TODO: Fix this clone.
            let cfg = function.borrow().clone();
            let function = self.lower_function(cfg);
            self.lir.functions.push(function);
        }

        /* Emit cleanup */
        self.lir
    }

    fn emit_prologue(&mut self, cfg: &CFG<'a>) -> Block {
        Block { // TODO: This needs to have .prologue appended.
            label: Label::Label(cfg.name),
            instructions: Vec::new(),
            optimize: false,
        }
    }

    fn emit_epilogue(&mut self, cfg: &CFG<'a>) -> Block {
        Block { // TODO: This needs to have .prologue appended.
            label: Label::Label(cfg.name),
            instructions: Vec::new(),
            optimize: false,
        }
    }

    /* Iterates over the basic blocks and lowers them to lc3-lir */
    fn lower_function(&mut self, cfg: CFG<'a>) -> Function {
        let prologue = self.emit_prologue(&cfg);
        let epilogue = self.emit_epilogue(&cfg);

        let mut func = Function {
            name: cfg.name,
            blocks: Vec::new(),
            setup: prologue,
            teardown: epilogue,
            optimize: false,
            stack_frame: HashMap::new(), // Map allocas to stack space.
        };

        for basic_block_h in &cfg.basic_block_order {
            let block = self.lower_bb(&cfg, &basic_block_h);
            func.blocks.push(block);
        }
        func
    }

    /* Iterates over the istructions in the block and lowers them to lc3-ir grouped into a Block? */
    fn lower_bb(&mut self, cfg: &CFG<'a>, basic_block_h: & BasicBlockHandle) -> Block {
        let instructions = Vec::new();

        for instruction_h in &cfg.resolve_bb(*basic_block_h).instructions {
            // Need to automatically add the ordering to the instructions vec. Oops!
            let inst = self.lower_hir_inst(cfg, *instruction_h);
            //instructions.push(inst);
        }


        Block { // TODO: This needs to have .prologue appended.
            label: Label::Label(cfg.name),
            instructions,
            optimize: false,
        }
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstHandle {
        self.lir.instruction_arena.insert(inst)
    }

    fn get_virtual_reg(&mut self, hir_instruction_h: InstructionHandle) -> Register {
        let a = self.hreg_to_lreg.get(&hir_instruction_h).expect("hir_instruction hasn't been lowered yet? HOW IS THIS POSSIBLE?!");
        (*a).into()
    }

    fn get_memory_location(&mut self, hir_instruction_h: InstructionHandle) -> MemoryLocation {

    }

    /* Can't do a recursive backwards dependency traversal from the return node becasue then we might accidentally do D.C.E and 
    also skip over instructions that modify global state. Also hard to deal w/ conditionals */
    /* Lower instructions in linear order */
    fn lower_hir_inst(&mut self, cfg: &CFG<'a>, instruction_h: InstructionHandle) -> () {
        let instruction = cfg.get_inst(&instruction_h);
        match instruction {
            // TODO: This shouldnt be here!
            /* 
            Instruction::Allocate(size) => {
                // These should really be constants, maybe a warning or something.
                // Whatever, we will handle it just as a thought experiment.
                let op = self.lower_hir_op(size);
                match op {
                    RegisterOrImmediate::Register(reg) => {
                        let inst = Inst::AddReg(Register::StackPointer, Register::StackPointer, reg); 
                        // This is silly so many .into()s
                        let reg: Register = self.add_inst(inst).into();
                        return reg.into();
                    }
                    RegisterOrImmediate::Immediate(imm) => {
   
                    }
                }
                todo!()
                
            }, */
            Instruction::Allocate(_) => todo!("unexpected allocate isntruction"),
            Instruction::Parameter => todo!("unexpected parameter instruction"),
            Instruction::Load(op) => {
                // TODO: Check where we are loading from!.
                // For stack, emit LDR
                // For global, emit LD 
                //
            },
            Instruction::LoadOffset(_, _) => todo!(),
            Instruction::Store(_, _) => todo!(),
            Instruction::StoreOffset(_, _, _) => todo!(),
            Instruction::BinaryOp(op, lhs, rhs) => {
                /* They really shouldn't both be immidiates! */
                let lhs = self.lower_hir_op(*lhs);
                let rhs = self.lower_hir_op(*rhs);
                
                /* Should we do optimizations here or save for later. They would work so well here! */
                let res = match op {
                    hir::BinaryOpType::Add => {
                        let inst = Inst::Add(lhs, rhs);
                        let inst= self.add_inst(inst);
                        inst
                    }
                    hir::BinaryOpType::Sub => {
                        let lhs = self.add_inst(Inst::Add(lhs, (-1).into())); // This is awesome that this works, howeve rcan't we just spam .into() automatically? 
                        // ^^^ Why do i have to call into. I hope it doesn't actually compile to anything
                        let lhs: Register = self.add_inst(Inst::Not(lhs.into())).into();
                        // Just kidding, this is a mess! wtf.
                        let res = self.add_inst(Inst::Add(lhs.into(), rhs));
                        res
                        
                        // Seems to only call into() automcatcally on function parameters / returns, wonder if we can get it to do it for struct / enum constructors.
                    }
                    hir::BinaryOpType::And => {
                        let inst = self.add_inst(Inst::And(lhs, rhs));
                        inst
                    }

                    // Nope:!
                    hir::BinaryOpType::Mul => todo!(),
                    hir::BinaryOpType::Div => todo!(),
                    hir::BinaryOpType::Mod => todo!(),
                    hir::BinaryOpType::Or => todo!(),
                    hir::BinaryOpType::LessThan => todo!(),
                    hir::BinaryOpType::GreaterThan => todo!(),
                    hir::BinaryOpType::LessThanEqual => todo!(),
                    hir::BinaryOpType::GreaterThanEqual => todo!(),
                    hir::BinaryOpType::NotEqual => todo!(),
                    hir::BinaryOpType::EqualEqual => todo!(),
                    hir::BinaryOpType::LeftShift => todo!(),
                    hir::BinaryOpType::RightShift => todo!(),
                };
                self.hreg_to_lreg.insert(instruction_h, res);

            }
            Instruction::UnaryOp(_, _) => todo!(),
            Instruction::CondBr(_, _, _) => todo!(),
            Instruction::Br(_) => todo!(),
            Instruction::Return(op) => {
                // Store value in return slot, 
                // Jump to teardown.
            }
            Instruction::Call(_) => todo!(),
        }
    }

    /* 
    fn move_to_vreg(&mut self, operand: RegisterOrImmediate) -> Register {
        match operand {
            RegisterOrImmediate::Register(reg) => reg,
            RegisterOrImmediate::Immediate(_) => {
                /* zero out a register. */
                let inst = Inst::And(Register::VRegister(()), 0)
                self.
                Register
            }
        }
    }  */

    // Can either be Register (InstHandle) or Immediate
    /*  */
    fn lower_hir_op(&mut self, operand: Operand) -> RegisterOrImmediate {
        // If it has already been lowered, then we can just get the virtual reg that is was mapped to, otherwise we need to lower it?
        match operand {
            Operand::Instruction(instruction_handle) => {
                self.get_virtual_reg(instruction_handle).into()
            }
            Operand::Const(value) => return value.into()
        }

    }
}