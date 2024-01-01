use std::{cell::RefCell, rc::Rc, collections::HashMap, borrow::Borrow};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, InstructionHandle, self, MemoryLocation}, lir::{LIR, Block, Inst, Register, RegisterOrImmediate, Immediate, InstHandle, Subroutine, Label, BlockHandle}};

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

struct LIRGenFunc<'ctx, 'ir> where 'ir: 'ctx {
    

    // HIR to LIR register map
    hreg_to_lreg: SecondaryMap<InstructionHandle, InstHandle>, // We aren't doin ga recursive traversal to lower so we need this mapping!

    // BB to Block Map
    bb_to_block: SecondaryMap<BasicBlockHandle, BlockHandle>, // This is annoying

    subroutine: Subroutine,
    cfg: CFG<'ir>,

    lir: &'ir mut LIR<'ir>,
    hir: &'ir mut HIR<'ir>,
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
impl <'ctx, 'ir> LIRGenFunc<'ctx, 'ir> {
    pub fn new(
        cfg: CFG<'ir>, 
        lir: &'ir mut LIR<'ir>,
        hir: &'ir mut HIR<'ir>,
        context: &'ctx Context<'ctx>
    ) -> LIRGenFunc<'ctx, 'ir>  {

        LIRGenFunc {
            subroutine: Subroutine::new(cfg.name),
            hreg_to_lreg: SecondaryMap::new(), 
            bb_to_block: SecondaryMap::new(),
            cfg,  
            lir, 
            hir, 
            context }
    }

    /* Iterates over the basic blocks and lowers them to lc3-lir */
    fn lower_subroutine(mut self) -> Subroutine {

        /* Get a mapping, then actually generate code for them */
        let order = self.cfg.basic_block_order;

        
        for basic_block_h in &order {
            let name = self.cfg.basic_block_arena.get(*basic_block_h).unwrap().name;
            let block_h = self.subroutine.block_arena.insert(
                Block { label: Label::Label(name), instructions: Vec::new(), optimize: true }
            );

            self.bb_to_block.insert(*basic_block_h, block_h);
        }

        for basic_block_h in &order {
            /* THIS DOESN"T USE .basic_block_order or .cfg, stop complainign PELASE  */

            self.lower_bb(&basic_block_h);
        }

        self.subroutine
    }

    /* Iterates over the istructions in the block and lowers them to lc3-ir grouped into a Block? */
    fn lower_bb(&mut self, basic_block_h: &BasicBlockHandle) -> () {
        let mut instructions = Vec::new();

        let cfg = &self.cfg;

        /* Loop through instructions and lower each instruction */
        
        for instruction_h in self.cfg.resolve_bb(*basic_block_h).instructions {
            // Need to automatically add the ordering to the instructions vec. Oops!
            let inst = self.lower_hir_inst(instruction_h);
            instructions.push(inst);
        }

        let block = self.bb_to_block.get_mut(*basic_block_h).unwrap();      
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstHandle {
        self.subroutine.inst_arena.insert(inst)
    }

    fn get_virtual_reg(&mut self, hir_instruction_h: InstructionHandle) -> Register {

        let a = self.hreg_to_lreg.get(hir_instruction_h).expect("hir_instruction hasn't been lowered");
        (*a).into()
    }


    fn materialize_constant(&mut self, imm: Immediate) -> Register {
        Register::VRegister(self.add_inst(Inst::Add(Register::ZeroReg, imm.into())))
    }

    fn into_reg (&mut self, rhs: RegisterOrImmediate) -> Register {
        match rhs {
            RegisterOrImmediate::Register(r) => r,
            RegisterOrImmediate::Immediate(v) => self.materialize_constant(v)
        }
    }

    fn destruct (&mut self, rhs: RegisterOrImmediate, lhs: RegisterOrImmediate) -> (Register, RegisterOrImmediate) {
        use RegisterOrImmediate as RoI;

        match (lhs.clone(), rhs.clone()) {
            (RoI::Register(l), RoI::Register(_)) => (l, rhs),
            (RoI::Register(l), RoI::Immediate(_)) => (l, rhs),
            (RoI::Immediate(_), RoI::Register(r)) => (r, lhs),
            (RoI::Immediate(_), RoI::Immediate(_)) => {
                // TODO: write a subroutine that materializes immediates 
                (self.into_reg(lhs), self.into_reg(rhs).into())
                
            }, 
        }
    } 

    /* Can't do a recursive backwards dependency traversal from the return node becasue then we might accidentally do D.C.E and 
    also skip over instructions that modify global state. Also hard to deal w/ conditionals */
    /* Lower instructions in linear order */
    fn lower_hir_inst(&mut self, instruction_h: InstructionHandle) -> InstHandle {
        let instruction = self.cfg.get_inst(&instruction_h);
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
                        let inst = self.subroutine.inst_arena.insert_with_key(f);
                        inst
                        
                    }
                    MemoryLocation::Parameter(size, offset) => {
                        let f = |op: InstHandle| {
                            Inst::Ldr(op.into(), Register::FramePointer, 4 + (offset as i32))
                        };
                        let inst = self.subroutine.inst_arena.insert_with_key(f);
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
                self.hreg_to_lreg.insert(instruction_h, inst);
                inst
            },
            Instruction::Store(op, loc) => {
                let op = self.lower_hir_op(*op);
                let op = self.into_reg(op);

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

                let inst = self.add_inst(inst);
                self.hreg_to_lreg.insert(instruction_h, inst);
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
                        let (reg, imm) = self.destruct(lhs, rhs);
                        let inst = Inst::Add(reg, imm);
                        let inst= self.add_inst(inst);
                        inst
                    }
                    hir::BinaryOpType::Sub => {
                        let lhs = self.into_reg(lhs);
                        let lhs = self.add_inst(Inst::Add(lhs.into(), (-1).into())); // This is awesome that this works, howeve rcan't we just spam .into() automatically? 
                        // ^^^ Why do i have to call into. I hope it doesn't actually compile to anything
                        let lhs: Register = self.add_inst(Inst::Not(lhs.into())).into();
                        // Just kidding, this is a mess! wtf.
                        let res = self.add_inst(Inst::Add(lhs.into(), rhs));
                        res
                        
                        // Seems to only call into() automcatcally on subroutine parameters / returns, wonder if we can get it to do it for struct / enum constructors.
                    }
                    hir::BinaryOpType::And => {
                        let lhs = self.into_reg(lhs);
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
            let fgen = LIRGenFunc::new(cfg, &mut self.lir, &mut self.hir , self.context);
           
            let subroutine =  fgen.lower_subroutine();
            self.lir.subroutines.push(subroutine);
        }

        /* Emit cleanup */
        self.lir
    }

    fn emit_prologue(&mut self, cfg: &CFG<'ctx>) -> Block {
        Block { // TODO: This needs to have .prologue appended.
            label: Label::Label(cfg.name),
            instructions: Vec::new(),
            optimize: false,
        }
    }

    /*  */
    fn emit_epilogue(&mut self, cfg: &CFG<'ctx>) -> Block {
        Block { // TODO: This needs to have .epologue appended.
            label: Label::Label(cfg.name),
            instructions: Vec::new(),
            optimize: false,
        }
    }

    
}