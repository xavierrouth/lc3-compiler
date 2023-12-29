use std::{cell::RefCell, rc::Rc, collections::HashMap, borrow::Borrow};

use crate::{TypedArena, hir::{HIR, CFG, BasicBlock, BasicBlockHandle, Instruction, Operand, InstructionHandle, self, MemoryLocation}, lir::{LIR, Block, Inst, Register, RegisterOrImmediate, Immediate, InstHandle, Function, Label, BlockHandle}};

use analysis::{symtab::{SymbolTable, ScopeHandle, VarDecl}, typedast::{TypedAST, TypedASTNode, TypedASTNodeHandle}};
use lex_parse::{ast::{ASTNodeHandle, AST, ASTNode, self}, error::ErrorHandler, context::Context};
use slotmap::{SparseSecondaryMap, SlotMap, SecondaryMap};

/* Lowering pass from HIR -> LIR, does not generate a completely valid LC3 instructons, needs to be register allocated, 
*  and validated (immediate sizes, imm, imm operands, etc.) */
/* Bascially LIR is invalid LC3, they mostly are the same besides that. */
pub struct LIRGen<'ctx> {
    
    // This is just codegen information for a single function. 

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

    function: Function,

    lir: &'ctx mut LIR<'ctx>,
    hir: &'ctx mut HIR<'ctx>,
    context: &'ctx Context<'ctx>,
}

impl 
impl <'ctx> LIRGen<'ctx> {
    pub fn new(hir: HIR<'ctx>, context: &'ctx Context<'ctx>) -> LIRGen<'ctx> {
        LIRGen {hir, context, 
            lir: LIR::new(context),
            bb_to_block: SecondaryMap::new(),
            hreg_to_lreg: SecondaryMap::new(),
            curr_function: None,
        }
    }

    pub fn prepare_cfg(&mut self, cfg: &mut CFG<'ctx>) -> () {
        ()
    }

    pub fn run(mut self) -> LIR<'ctx> {
        /* Emit setup */

        /* Lower each function */
        for function in self.hir.functions {

            self.hreg_to_lreg.clear();
            self.bb_to_block.clear();

            let cfg =  function.as_ref().borrow().to_owned();
            let function = self.lower_function(cfg);
            self.lir.functions.push(function);
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

    /* Iterates over the basic blocks and lowers them to lc3-lir */
    fn lower_function(&mut self, cfg: CFG<'ctx>) -> Function {
        let mut block_arena: SlotMap<BlockHandle, Block> = SlotMap::with_key();

        let prologue = block_arena.insert(self.emit_prologue(&cfg));
        let epilogue = block_arena.insert(self.emit_epilogue(&cfg));

        let mut func = Function {
            name: cfg.name,
            setup: prologue,
            teardown: epilogue,
            optimize: false,
            block_arena: block_arena,
            inst_arena: SlotMap::with_key(),
        };

        /* Get a mapping, then actually generate code for them */
        for basic_block_h in &cfg.basic_block_order {
            let name = cfg.basic_block_arena.get(*basic_block_h).unwrap().name;
            let block_h = func.block_arena.insert(
                Block { label: Label::Label(name), instructions: Vec::new(), optimize: true }
            );
            self.bb_to_block.insert(*basic_block_h, block_h);
        }

        for basic_block_h in &cfg.basic_block_order {
            self.lower_bb(&cfg, &basic_block_h);
        }

        func
    }

    /* Iterates over the istructions in the block and lowers them to lc3-ir grouped into a Block? */
    fn lower_bb(&mut self, cfg: &CFG<'ctx>, basic_block_h: & BasicBlockHandle) -> () {
        let mut instructions = Vec::new();

        for instruction_h in &cfg.resolve_bb(*basic_block_h).instructions {
            // Need to automatically add the ordering to the instructions vec. Oops!
            let inst = self.lower_hir_inst(cfg, *instruction_h);
            instructions.push(inst);
        }

        let block = self.bb_to_block.get_mut(*basic_block_h).unwrap();
        
        cfg.;
        
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstHandle {
        self.curr_function.unwrap().inst_arena.insert(inst)
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
                // TODO: write a function that materializes immediates 
                (self.into_reg(lhs), self.into_reg(rhs).into())
                
            }, 
        }
    } 

    /* Can't do a recursive backwards dependency traversal from the return node becasue then we might accidentally do D.C.E and 
    also skip over instructions that modify global state. Also hard to deal w/ conditionals */
    /* Lower instructions in linear order */
    fn lower_hir_inst(&mut self, cfg: &CFG<'ctx>, instruction_h: InstructionHandle) -> () {
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
                        let inst = self.lir.instruction_arena.insert_with_key(f);
                        inst
                        
                    }
                    MemoryLocation::Parameter(size, offset) => {
                        let f = |op: InstHandle| {
                            Inst::Ldr(op.into(), Register::FramePointer, 4 + (offset as i32))
                        };
                        let inst = self.lir.instruction_arena.insert_with_key(f);
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
                        
                        // Seems to only call into() automcatcally on function parameters / returns, wonder if we can get it to do it for struct / enum constructors.
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

            }
            Instruction::UnaryOp(_, _) => todo!(),
            Instruction::CondBr(_, _, _) => todo!(),
            Instruction::Br(bb) => {
                
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