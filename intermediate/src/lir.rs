
/** Basically LC3 at list point, should be pretty darn close. */
/* Not sure what the practical difference is between this and the /codegen instructions, maybe this isn't verified, 
but everything should be target specific at this point (so its codegen'd, not really intermediate). 
* Honestly lets just print a verified version of this. */

/* Here are some goals about this IR:
 * No information about subroutines, that doesn't exist, we only have labels, instructions, and data.
 * Starts in SSA and then register allocation to non-ssa? Probably somethjing like that makes sense.
 * 
 * Some optimizations / passes we want to do here

 * 
 *  Peephole optimizations (is this just pattern matching again kinda)
 *  Register Allocation
 *  callee save only used regs, calleer save only used regs
 * 
 *  Pattern matching instruction selection? (what does this even mean lol)
 *  What else is there to do?
 * 
 *  Is this just an instruction DAG  for each BasicBlock -> Block?
 * 
 * Some things we want to add eventually might be, (need to support blocks referencing each other and such)
 * - block reordering based on condition code analysis or something, optimize fall throughs
 * - 
 * - 
 * 
 * As a reference, these are some passes LLVM does on their machine IR:
 * BEFORE REG ALLOC
 * - Machine Common Subexpression Elimination
 * - Machine LICM
 * - Peephole Optimizer
 * - Machine DCE
 * 
 * AFTER REG ALLOC
 * - Prolog / Epilog insertion
 * - Machine Instruction Scheduling (2nd time scheudling)
 * - Block Placement
 * 
 */

 /* At this point give up on trying to do analysis on anything that is in memory (overflowed things). That is a completely reasonable decision right? 
  * */

use std::{fmt::Display, cell::RefCell};

use lex_parse::{context::{InternedString, Context}, ast::WithContext};
use slotmap::{SlotMap, SecondaryMap};

slotmap::new_key_type! { pub struct InstHandle; }
slotmap::new_key_type! { pub struct BlockHandle; }

#[derive(Debug, Clone)]
pub struct LIR<'ctx> {

    // Pre register allocation
    pub subroutines: Vec<Subroutine>,

    // Data section, 
    pub data: Vec<Data>,

    pub context: &'ctx Context<'ctx>,
}

impl <'ctx> LIR <'ctx> {
    pub fn print(&self) -> () {
        for sub in &self.subroutines {
            let printable = SubroutinePrintable::new(sub.clone(), self.context);
            printable.print();
        }
    }

    pub fn new(context: &'ctx Context<'ctx>) -> LIR<'ctx> {
        LIR {
            subroutines: Vec::new(), 
            data: Vec::new(),
            context,
        }
    }
}

/* ======== Stack Frame ============ */
#[derive(Debug, Clone)]
pub struct StackFrame {
    parameters_size: usize,
    locals_size: usize,
}

/* ======== Subroutine ============ */
/* Eventually we could have different subroutine types that support different calling converntions,
*  This represents a subroutine with the classic C calling convention. */
#[derive(Debug, Clone)]
pub struct Subroutine {
    pub(crate) name: InternedString,

    pub block_order: Vec<BlockHandle>,

    pub block_arena: SlotMap<BlockHandle, Block>,

    // TODO: Should this be owned by the Blocks themselves, 
    // that makes it really difficult to merge blocks, or move instructions in between blocks, 
    // so I think that is a bad idea in general.
    
    pub inst_arena: SlotMap<InstHandle, Inst>,

    pub(crate) body: BlockHandle,
    pub(crate) setup: Option<BlockHandle>,
    pub(crate) teardown: Option<BlockHandle>,
    pub(crate) optimize: bool,

    /* TODO: Add a field represnting the stack frame maybe? Mapping certain ops to memory locations. */
    stack_frame: StackFrame,
    
}

/* Try a pattern like this? */

impl Subroutine {
    pub fn new(name: InternedString) -> Subroutine {
        let mut block_arena = SlotMap::with_key();
 
        let body = Block {
            label: Label::Unnamed,
            instruction_order: Vec::new(),
            optimize: true,
        };

        let body = block_arena.insert(body);
        Subroutine {
            name, 
            block_order: Vec::new(),
            block_arena: SlotMap::with_key(),
            inst_arena: SlotMap::with_key(), 
            setup: None,
            teardown: None,
            optimize: true,
            stack_frame: 
                StackFrame {
                    parameters_size: 0,
                    locals_size: 0,
                },
            body, 
        }
    }
    
    
    /** These can be all part of a 'Block Builder', that has a ref to Subroutine, for convenience. */
    pub fn add_inst(&mut self, block_h: &BlockHandle, inst: Inst, ) -> InstHandle {
        let block = self.block_arena.get_mut(*block_h).unwrap();
        let h = self.inst_arena.insert(inst);
        block.instruction_order.push(h);
        h
    }

    /* 
    pub fn add_inst_fast(&mut self, block: &mut Block, inst: Inst,) -> InstHandle {
        let h = self.inst_arena.insert(inst);
        block.instruction_order.push(h);
        h
    }

    pub fn materialize_constant(&mut self, block_h: &BlockHandle, imm: Immediate) -> Register {
        Register::VRegister(self.add_inst(block_h, Inst::Add(Register::ZeroReg, imm.into())))
    }

    pub fn into_reg (&mut self, rhs: RegisterOrImmediate) -> Register {
        match rhs {
            RegisterOrImmediate::Register(r) => r,
            RegisterOrImmediate::Immediate(v) => self.materialize_constant(v)
        }
    }

    pub fn destruct (&mut self, rhs: RegisterOrImmediate, lhs: RegisterOrImmediate) -> (Register, RegisterOrImmediate) {
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
    }  */
}

/* What if we need to partially borrow.... Let's see what happens.,  */
pub struct SubroutineBuilder<'a> {
    inst_arena: &'a mut SlotMap<InstHandle, Inst>,
    handle: BlockHandle,
    block: &'a mut Block, 
}

impl <'a> SubroutineBuilder<'a> {
    pub fn new(inst_arena: &'a mut SlotMap<InstHandle, Inst>, handle: &BlockHandle, block: &'a mut Block) -> Self {
        SubroutineBuilder {
            inst_arena,
            handle: *handle, 
            block: block,
        }
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstHandle {
        let h = self.inst_arena.insert(inst);
        self.block.instruction_order.push(h);
        h
    }

    pub fn add_inst_with_key(&mut self, f: impl Fn(InstHandle) -> Inst) -> InstHandle {
        let h = self.inst_arena.insert_with_key(f);
        self.block.instruction_order.push(h);
        h
    }

    pub fn materialize_constant(&mut self, imm: Immediate) -> Register {
        Register::VRegister(self.add_inst(Inst::Add(Register::ZeroReg, imm.into())))
    }

    pub fn into_reg (&mut self, rhs: RegisterOrImmediate) -> Register {
        match rhs {
            RegisterOrImmediate::Register(r) => r,
            RegisterOrImmediate::Immediate(v) => self.materialize_constant(v)
        }
    }

    pub fn destruct (&mut self, rhs: RegisterOrImmediate, lhs: RegisterOrImmediate) -> (Register, RegisterOrImmediate) {
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
}

/* ============= Subroutine Printable ============ */
pub struct SubroutinePrintable<'ctx> {
    pub context: &'ctx Context<'ctx>,
    pub subroutine: Subroutine,

    pub names: RefCell<SecondaryMap<InstHandle, i32>>,
    pub counter: RefCell<i32>,
}

impl <'ctx> SubroutinePrintable<'ctx> {
    pub fn new(subroutine: Subroutine, context: &'ctx Context<'ctx>) -> SubroutinePrintable<'ctx> {
        SubroutinePrintable {
            subroutine,
            context,
            names: RefCell::new(SecondaryMap::new()), 
            counter: RefCell::new(0)
        }
    } 

    pub fn print(&self) {
        let name = self.context.resolve_string(self.subroutine.name);
        println!("subroutine: {}", name);

        /* Why isn't this doing anything?  */
        for block_handle in &self.subroutine.block_order {
            self.print_block(block_handle);
        }
    }

    fn print_block(&self, block_handle: &BlockHandle) -> () {
        let block = self.subroutine.block_arena.get(*block_handle).expect("uh oh");
        let name = match block.label {
            Label::Label(l) => self.context.resolve_string(l),
            Label::Unnamed => "unnamed".to_owned(),
        };

        println!("block {}:", name);

        for inst in &block.instruction_order {
            self.print_inst(inst);
        } 
        println!("");  
    }

    fn print_inst(&self, inst_handle: &InstHandle) -> () {
        let inst = self.subroutine.inst_arena.get(*inst_handle).unwrap();
        
        let out: String = match inst {
            Inst::Add(r1, r2) => {
                format!("{} = add {}, {}", 
                    self.name_inst(inst_handle), 
                    self.get_reg_name(r1), 
                    self.get_reg_immediate_name(r2)
                )
            },
            Inst::And(r1, r2) => {
                format!("{} = and {}, {}", 
                    self.name_inst(inst_handle), 
                    self.get_reg_name(r1), 
                    self.get_reg_immediate_name(r2)
                )
            },
            Inst::Not(r1) => {
                format!("{} = not {}", 
                    self.name_inst(inst_handle), 
                    self.get_reg_name(r1), 
                )
            }
            Inst::St(_, _) => todo!(),
            Inst::Sti(_, _) => todo!(),
            Inst::Str(_, _, _) => todo!(),
            Inst::Ld(_, _) => todo!(),
            Inst::Ldi(_, _) => todo!(),
            Inst::Ldr(_, _, _) => todo!(),
            Inst::Lea(_, _) => todo!(),

            Inst::Br(cc, label) => {
                format!("bingle")
                /*
                format!("{} = not {}", 
                    self.name_inst(inst_handle), 
                    self.get_reg_name(r1), 
                )  */
            },

            Inst::BrImm(_, _) => todo!(),
            Inst::Jmp(_) => todo!(),
            Inst::Jsr(_) => todo!(),
            Inst::Jsrr(_) => todo!(),
            Inst::Ret => todo!(),
            Inst::Rti => todo!(),
            Inst::Trap(_) => todo!(),
            Inst::Halt => todo!(),
            Inst::SillySubroutine(_) => todo!(),
        };
        println!("   {out}");
    }

    fn get_reg_immediate_name(&self, roi: &RegisterOrImmediate) -> String {
        match roi {
            RegisterOrImmediate::Register(r) => self.get_reg_name(r),
            RegisterOrImmediate::Immediate(imm) => format!("{imm}"),
        }
    }

    /* Assigns a name to an InstHandle */
    fn name_inst(& self, inst: &InstHandle) -> String {
        let mut names = self.names.borrow_mut();
        let name = names.get(*inst);
        match name {
            Some(&val) => {
                format!("v%{val}")
            }
            None => {
                // Mutable RefMut ????? 
                let mut ctr = self.counter.borrow_mut();
                let tmp = ctr.clone();
                names.insert(*inst, *ctr);
                *ctr += 1;
                format!("v%{tmp}")
            }
        }
    }

    fn get_reg_name(&self, register: &Register) -> String {
        // Does this involve a copy?
        match register {
            Register::VRegister(inst) => {
                /* Name the instruction if needed. */
                /* after moving this to outer loop, then: 
                INSTRUCTION SHOULD BE NAMED ALREADY, ELSE REORDERING CAUSE A PROBLEM. */

                // TODO, we can name these earlier to remove a conciditoonal check. 
                self.name_inst(inst)
            }
            Register::ZeroReg => format!("%R(zero)"),
            Register::HRegister(val) => {
                format!("%R{val}")
            },
            Register::StackPointer => format!("%SP"),
            Register::FramePointer => format!("%FP"),
        }
       
    }

}

/* ========== Block ============ */
/* Not a basic block, just a block lol. LIR analogue of HIR's BasicBlock */
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub(crate) label: Label,
    pub(crate) instruction_order: Vec<InstHandle>,
    pub(crate) optimize: bool, // Whether we are allowed to touch this for optimizations.
}

#[derive(Debug, Clone, PartialEq)]
pub enum Label {
    Label(InternedString),
    Unnamed,
}

/* TODO: Potentially support inlining of these. */
#[derive(Debug, Clone, PartialEq)]
pub enum SubroutineKind {
    Multiply(Register, Register), // TODO: Different versions based on sign.
    Divide(Register, Register),
    Lshift(Register, Register),
    Rshift(Register, Register),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TrapKind {

}

pub type Immediate = i32;

#[derive(Debug, Clone, PartialEq)]
pub enum Register {
    // Zero Register is a virtual Register aswell, 
    /* Virtual Register, infinite  */
    VRegister(InstHandle), 
    ZeroReg,

    /* Hardware Register, in between 0 and 3 probably */
    HRegister(u8), 
    StackPointer, /* StackPointer should just be an alias for HRegister(8) */
    FramePointer,
    
} 


/* Some handy conversion functions. */
impl From<InstHandle> for Register {
    fn from(value: InstHandle) -> Self {
        Register::VRegister(value)
    }
}

// I have re-invented pointer object-oriented downcasts?
impl TryFrom<RegisterOrImmediate> for Register {
    type Error = &'static str;

    fn try_from(value: RegisterOrImmediate) -> Result<Self, Self::Error> {
        match value {
            RegisterOrImmediate::Register(value) =>  Ok(value),
            RegisterOrImmediate::Immediate(_) => Err("Bad!"),
        }
    }
}

impl TryFrom<RegisterOrImmediate> for Immediate {
    type Error = &'static str;

    fn try_from(value: RegisterOrImmediate) -> Result<Self, Self::Error> {
        match value {
            RegisterOrImmediate::Register(_) => Err("Bad!"),
            RegisterOrImmediate::Immediate(value) => Ok(value),
        }
    }
}

/* At some point we need to decide if this will actually be access using global register ptr (R4), or with just names and hope they are close enough. */
#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    String(InternedString),
    Array(),
    Empty(),
    Struct() // Don't support these for now.
}

// Surely there is a way to implement this automatically
#[derive(Debug, Clone, PartialEq)]
pub enum RegisterOrImmediate {
    Register(Register),
    Immediate(Immediate),
}

impl From<Register> for RegisterOrImmediate {
    fn from(value: Register) -> Self {
        RegisterOrImmediate::Register(value)
    }
}

impl From<Immediate> for RegisterOrImmediate {
    fn from(value: Immediate) -> Self {
        RegisterOrImmediate::Immediate(value)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum BrCondCode {
    NZP,
    NZ,
    ZP,
    NP,
    N,
    P,
    Z,
}
/* ============= Inst ============== */
/* We use Inst in LIR, instead of Instruction (in HIR), because Inst is smaller and thus closer to the assembly. */
/* How to model dataflow? Do we even need to at this stage? (Yes, we want that). */

/* TODO: I want to eliminate the possibility that lc3 instructions that don't output into a register are used as ops,
*  I'm not sure how to do that yet, probably just by messing with how these types are set up */
#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /* Dataflow Instructions (Should we remove result register?) */ 
    /* Unclear if it makes sense to distinguish between register vs immediates here,  */
    Add(Register, RegisterOrImmediate),
    And(Register, RegisterOrImmediate),

        //AddReg(Register, Register),
        //AddImm(Register, Immediate),
        //AndReg(Register, Register),
        //AndImm(Register, Immediate),
    
    Not(Register),

    /* Memory Instructions */
    St(Register, Label),
    Sti(Register, Label), // Indirect Store (use for casts)
    Str(Register, Register, Immediate), // Source, Base, Offset
    Ld(Register, Label), // Should be Label or immediate, but almost always Label
    Ldi(Register, Label),
    Ldr(Register, Register, Immediate),
    Lea(Register, Label),

    /* Control-flow instructions */
    Br(BrCondCode, Label),
    BrImm(BrCondCode, Immediate),
    Jmp(Register),
    Jsr(Label),
    Jsrr(Register),
    
    
    Ret,
    Rti, // What?
    
    Trap(TrapKind), // Trap vector.
    Halt, // We should really sub-enum trap above ^^^
    SillySubroutine(SubroutineKind), /* FIX NAMES !s */
}