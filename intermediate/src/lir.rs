
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

use lex_parse::context::{InternedString, Context};
use slotmap::SlotMap;

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
        println!("hi bingle :)")
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

    // TODO: We don't have a block ordering yet. 
    // pub(crate) blocks: Vec<Block>,

    pub block_arena: SlotMap<BlockHandle, Block>,
    pub inst_arena: SlotMap<InstHandle, Inst>,

    pub(crate) setup: Option<BlockHandle>,
    pub(crate) teardown: Option<BlockHandle>,
    pub(crate) optimize: bool,

    /* TODO: Add a field represnting the stack frame maybe? Mapping certain ops to memory locations. */
    stack_frame: StackFrame,
    
}

impl Subroutine {
    pub fn new(name: InternedString) -> Subroutine {
        Subroutine {
            name, 
            block_arena: SlotMap::with_key(),
            inst_arena: SlotMap::with_key(),
            setup: None,
            teardown: None,
            optimize: true,
            stack_frame: 
                StackFrame {
                    parameters_size: 0,
                    locals_size: 0,
                } 
        }
    }

    pub fn add_inst(&mut self, inst: Inst) -> InstHandle {
        self.inst_arena.insert(inst)
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

/* ========== Block ============ */
/* Not a basic block, just a block lol. LIR analogue of HIR's BasicBlock */
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub(crate) label: Label,
    pub(crate) instructions: Vec<InstHandle>,
    pub(crate) optimize: bool, // Whether we are allowed to touch this for optimizations.
}

#[derive(Debug, Clone, PartialEq)]
pub enum Label {
    Label(InternedString)
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
    Br(bool, bool, bool, Label),
    BrImm(bool, bool, bool, Immediate),
    Jmp(Register),
    Jsr(Label),
    Jsrr(Register),
    
    
    Ret,
    Rti, // What?
    
    Trap(TrapKind), // Trap vector.
    Halt, // We should really sub-enum trap above ^^^
    SillySubroutine(SubroutineKind), /* FIX NAMES !s */
}