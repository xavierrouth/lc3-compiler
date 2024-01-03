use std::{cell::RefCell, rc::Rc, collections::HashMap, borrow::Borrow};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, InstructionHandle, self, MemoryLocation}, lir::{LIR, Block, Inst, Register, RegisterOrImmediate, Immediate, InstHandle, Subroutine, Label, BlockHandle, SubroutineBuilder}};

use analysis::{symtab::{SymbolTable, ScopeHandle, VarDecl}, typedast::{TypedAST, TypedASTNode, TypedASTNodeHandle}};
use lex_parse::{ast::{ASTNodeHandle, AST, ASTNode, self}, error::ErrorHandler, context::Context};
use slotmap::{SparseSecondaryMap, SlotMap, SecondaryMap};

/* Lowering pass from HIR -> LIR, does not generate a completely valid LC3 instructons, needs to be register allocated, 
*  and validated (immediate sizes, imm, imm operands, etc.) */
/* Bascially LIR is invalid LC3, they mostly are the same besides that. */
pub struct LIRGen<'ctx> {
    
    // This is just codegen information for a single subroutine. 

    lir: LIR<'ctx>,

    // External Information
    hir: HIR<'ctx>,
    context: &'ctx Context<'ctx>,
}

struct LIRGenFunc<'ctx> {
    // HIR to LIR register map
    hreg_to_lreg: SecondaryMap<InstructionHandle, InstHandle>, // We aren't doin ga recursive traversal to lower so we need this mapping!

    // BB to Block Map
    bb_to_block: SecondaryMap<BasicBlockHandle, BlockHandle>, // This is annoying

    context: &'ctx Context<'ctx>,
}

/* NOTE: 
 * We store all information for lowering a function and the state of that in a struct 'LIRGenFunc', 
 * then methods on this struct do the lowering by modifying the internal state through 'self'.
 * 
 * Currently we are running into problems with borrowing parts of self while still wanting to mutably borrow other parts.
 * 
 * For example, we borrow the order of basic blocks, from the CFG in order to lower basic blocks, 
 * Then when lowering a basic block we need to mutably borrow the rest of self. 
 * 
 * Even though 
 * A more rust-y way to do this is to not use self to store state, instead use parameters
 * 
 */

/* Generator for a single function, so we don't have to pass state around through parameters.  */
// This doesn't work it seems, should use parameters instead. 
impl <'ctx> LIRGenFunc<'ctx> {

    pub fn new(
        context: &'ctx Context<'ctx>
    ) -> LIRGenFunc<'ctx>  {

        LIRGenFunc {
            hreg_to_lreg: SecondaryMap::new(), 
            bb_to_block: SecondaryMap::new(),
            context }
    }


    pub fn run(self, cfg: CFG<'ctx>) -> Subroutine {
        self.lower_subroutine(cfg)
    }
    
    /* Iterates over the basic blocks and lowers them to lc3-lir */
    fn lower_subroutine(mut self, mut cfg: CFG<'ctx>) -> Subroutine {

        /* Get a mapping, then actually generate code for them */
        let order = cfg.basic_block_order.clone()  ;

        let mut subroutine = Subroutine::new(cfg.name);

        /* Map HIR basic blocks to LIR blocks */
        for basic_block_h in &order {

            let name = cfg.basic_block_arena.get(*basic_block_h).unwrap().name;
            let block_h = subroutine.block_arena.insert (
                Block { label: Label::Label(name), instruction_order: Vec::new(), optimize: true }
            );

            self.bb_to_block.insert(*basic_block_h, block_h);
            /* This is so wrong,  */
            subroutine.block_order.push(block_h);
        }

        /* Populate LIR blocks */
        for basic_block_h in &order {
            /* THIS DOESN"T USE .basic_block_order or .cfg, stop complainign PELASE  */
            let block_h = self.bb_to_block.get(*basic_block_h).unwrap().clone();
            self.lower_bb((basic_block_h, &block_h), &mut cfg, &mut subroutine);
        }

        subroutine
    }


    /* Iterates over the istructions in the block and lowers them to lc3-ir grouped into a Block? */
    fn lower_bb(&mut self, block_info: (&BasicBlockHandle, &BlockHandle), cfg: &mut CFG<'ctx>, subroutine: &mut Subroutine) -> () {
        let (basic_block_h, block_h) = block_info;

        let block = subroutine.block_arena.get_mut(*block_h).unwrap();

        /* Loop through instructions and lower each instruction */
        let mut builder = SubroutineBuilder::new(&mut subroutine.inst_arena, block_h, block);

        for instruction_h in &cfg.resolve_bb(*basic_block_h).instructions {
            self.lower_hir_inst(instruction_h, cfg, &mut builder);
        }

    }


    fn get_virtual_reg(&mut self, hir_instruction_h: InstructionHandle) -> Register {

        let a = self.hreg_to_lreg.get(hir_instruction_h).expect("hir_instruction hasn't been lowered");
        (*a).into()
    }


    /* Can't do a recursive backwards dependency traversal from the return node becasue then we might accidentally do D.C.E and 
    also skip over instructions that modify global state. Also hard to deal w/ conditionals */
    /* Lower instructions in linear order */
    fn lower_hir_inst(& mut self, instruction_h: &InstructionHandle, cfg: & CFG<'ctx>,  builder: &mut SubroutineBuilder<'_>) -> InstHandle {
        let instruction = cfg.get_inst(&instruction_h);
        match instruction {
            Instruction::Load(loc) => {
                let inst = match *loc  {
                    MemoryLocation::Stack(size, offset) => {
                        // TODO: Loop through size in bytes. 
                        /* 
                        for i in (0 as usize)..*size  {
                            let inst = Inst::Str(op, Register::FramePointer, (offset + i).try_into().unwrap());
                            let inst = self.add_inst(inst);
                            self.hreg_to_lreg.insert(instruction_h, inst); // Will this lower in the right order ? Who cares. 
                            return
                        } */
                        let f = |op: InstHandle| {
                            Inst::Ldr(op.into(), Register::FramePointer, -1 * (offset as i32))
                        };
                        let inst = builder.add_inst_with_key(f);
                        inst
                        
                    }
                    MemoryLocation::Parameter(size, offset) => {
                        let f = |op: InstHandle| {
                            Inst::Ldr(op.into(), Register::FramePointer, 4 + (offset as i32))
                        };
                        let inst = builder.add_inst_with_key(f);
                        inst
                    }
                    // Wtf is the difference between label and data idiot. 
                    MemoryLocation::Data(_) => todo!(),
                    MemoryLocation::Label(l) => {
                        todo!();
                        //let inst = Inst::St(op, l);
                        //inst
                    }
                    MemoryLocation::Cast(_) => todo!(),
                };
                self.hreg_to_lreg.insert(*instruction_h, inst);
                inst
            },
            Instruction::Store(op, loc) => {
                let op = self.lower_hir_op(*op);
                let op = builder.into_reg(op);

                let inst = match *loc  {
                    MemoryLocation::Stack(size, offset) => {
                        // TODO: Loop through size in bytes. 
                        /* 
                        for i in (0 as usize)..*size  {
                            let inst = Inst::Str(op, Register::FramePointer, (offset + i).try_into().unwrap());
                            let inst = self.add_inst(inst);
                            self.hreg_to_lreg.insert(instruction_h, inst); // Will this lower in the right order ? Who cares. 
                            return
                        } */
                        let inst = Inst::Str(op, Register::FramePointer, -1 * (offset as i32));
                        inst
                        
                    }
                    MemoryLocation::Parameter(size, offset) => {
                        let inst = Inst::Str(op, Register::FramePointer, 4 + (offset as i32));
                        inst
                    }
                    // Wtf is the difference between label and data idiot. 
                    MemoryLocation::Data(_) => todo!(),
                    MemoryLocation::Label(l) => {
                        todo!();
                        //let inst = Inst::St(op, l);
                        //inst
                    }
                    MemoryLocation::Cast(_) => todo!(),
                };

                let inst = builder.add_inst(inst);
                self.hreg_to_lreg.insert(*instruction_h, inst);
                inst
            }
            Instruction::BinaryOp(op, lhs, rhs) => {
                /* They really shouldn't both be immidiates!, if lhs is an immediate then load it into a register and cry. */
                let lhs = self.lower_hir_op(*lhs);
                let rhs = self.lower_hir_op(*rhs);

                use RegisterOrImmediate as RoI;

                /* For commutative ops, confirm that at least one of the operands is a register, if not make it one */
                // TODO: Make sure this is like inlined or something idk? 

                /* Should we do optimizations here or save for later. They would work so well here! */
                let res = match op {
                    hir::BinaryOpType::Add => {
                        let (reg, imm) = builder.destruct(lhs, rhs);
                        let inst = Inst::Add(reg, imm);
                        let inst= builder.add_inst(inst);
                        inst
                    }
                    hir::BinaryOpType::Sub => {
                        let lhs = builder.into_reg(lhs);
                        let lhs = builder.add_inst(Inst::Add(lhs.into(), (-1).into())); // This is awesome that this works, howeve rcan't we just spam .into() automatically? 
                        // ^^^ Why do i have to call into. I hope it doesn't actually compile to anything
                        let lhs: Register = builder.add_inst(Inst::Not(lhs.into())).into();
                        // Just kidding, this is a mess! wtf.
                        let res = builder.add_inst(Inst::Add(lhs.into(), rhs));
                        res
                        
                        // Seems to only call into() automcatcally on subroutine parameters / returns, wonder if we can get it to do it for struct / enum constructors.
                    }
                    hir::BinaryOpType::And => {
                        let lhs = builder.into_reg(lhs);
                        let inst = builder.add_inst(Inst::And(lhs, rhs));
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
                self.hreg_to_lreg.insert(*instruction_h, res);
                res
            }
            Instruction::UnaryOp(_, _) => todo!(),
            Instruction::CondBr(_, _, _) => todo!(),
            Instruction::Br(bb) => {
                todo!()
            }
            Instruction::Return(op) => {
                // Store value in return slot, 
                // Jump to teardown.
                todo!()
            }
            Instruction::Call(_) => todo!(),
            Instruction::Lea(_) => todo!(),
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

impl <'ctx> LIRGen<'ctx> {
    pub fn new(hir: HIR<'ctx>, context: &'ctx Context<'ctx>) -> LIRGen<'ctx> {
        LIRGen {hir, context, 
            lir: LIR::new(context),
        }
    }
    

    pub fn prepare_cfg(&mut self, cfg: &mut CFG<'ctx>) -> () {
        ()
    }

    pub fn run(mut self) -> LIR<'ctx> {
        /* Emit setup */

        
        /* Lower each subroutine */
        for cfg in self.hir.functions {
            
            let cfg =  cfg.as_ref().borrow().to_owned();
            // TODO: Reuse LIRGENFunc instead of allocating and freeing new ones, is this something a compiler can do? 
            let fgen = LIRGenFunc::new(self.context);
           
            let subroutine =  fgen.run(cfg);
            self.lir.subroutines.push(subroutine);
        }

        /* Emit cleanup */
        self.lir
    }

    fn emit_prologue(&mut self, cfg: &CFG<'ctx>) -> Block {
        Block { // TODO: This needs to have .prologue appended.
            label: Label::Label(cfg.name),
            instruction_order: Vec::new(),
            optimize: false,
        }
    }

    /*  */
    fn emit_epilogue(&mut self, cfg: &CFG<'ctx>) -> Block {
        Block { // TODO: This needs to have .epologue appended.
            label: Label::Label(cfg.name),
            instruction_order: Vec::new(),
            optimize: false,
        }
    }

    
}