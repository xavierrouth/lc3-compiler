


use std::{cell::{RefCell, Cell}, rc::Rc, fmt::Display, alloc::Layout, collections::HashMap};

use crate::TypedArena;

use analysis::{typecheck::TypeCast, symbol_table::{SymbolTable, LocalVarDecl, ScopeHandle}};
use lex_parse::{ast::{ASTNodeHandle, AST}, error::ErrorHandler, context::{InternedString, Context}};
use slotmap::{SparseSecondaryMap, SlotMap, SecondaryMap};

// TODO: Should be consistent in naming of type data vs type handle. Which one gets the original name, which one gets the suffix?

slotmap::new_key_type! { pub struct InstructionHandle; }
slotmap::new_key_type! { pub struct BasicBlockHandle; }

/* High level IR, generated from AST, moved to SSA form, DCE,  */
pub struct HIR {
    pub main: Option<i32>,
    pub functions: Vec<Rc<RefCell<CFG>>>,
    pub data: Vec<DataEntry>,
}

pub struct DataEntry {
    size: u32,
    name: InternedString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogAnd, // Are Log and Bit / Bin the same??
    LogOr,
    BitAnd,
    BitOr,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    NotEqual,
    EqualEqual,

    LeftShift,
    RightShift,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpType {
    Negate,
    LogNot,
    BinNot, 
}

/* CFG for a single function */
#[derive(Debug, Clone)]
pub struct CFG {
    pub(crate) entry: BasicBlockHandle,
    pub(crate) name: InternedString,
    pub(crate) instruction_arena: SlotMap<InstructionHandle, Instruction>,
    pub(crate) basic_block_arena: SlotMap<BasicBlockHandle, BasicBlock>,

    // Stack Frame:
    parameters_offset: usize,
    locals_offset: usize,
    locals: HashMap<LocalVarDecl, MemoryLocation>, 
}

#[derive(Debug, Clone)]
struct LocalVarSlot {
    size: i32,
}

impl PartialEq for CFG {
    fn eq(&self, other: &Self) -> bool {
        self.entry == other.entry && self.name == other.name
    }
}


impl CFG {
    pub fn new(name: InternedString) -> CFG {
        let mut basic_block_arena = SlotMap::with_key();
        let entry = basic_block_arena.insert(BasicBlock { instructions: Vec::new(), terminator: None, incoming: Vec::new() });
        CFG {
            instruction_arena: SlotMap::with_key(),
            basic_block_arena,
            name,
            entry,
            parameters_offset: 0,
            locals_offset: 0,
            locals: HashMap::new(),
        }
    }

    pub fn get_location(&self, local: LocalVarDecl) -> MemoryLocation {
        if !(self.locals.contains_key(&local)) {
            panic!("variable does not belong to current function or hasn't been allocated yet.")
        }
        self.locals.entry(local)
    }

    pub fn add_parameter(&mut self, parameter: LocalVarDecl) -> () {
        self.locals.entry(parameter).or_insert(MemoryLocation::Parameter(self.parameters_offset));
        self.parameters_offset += parameter.size;
    }

    pub fn add_local(&mut self, local: LocalVarDecl, alloca: InstructionHandle) -> () { // Do we need scope information here? (scope: ScopeHandle)
        self.locals.entry(local).or_insert(MemoryLocation::Stack(alloca, self.locals_offset));
        self.locals_offset += local.size;
    }

    pub fn resolve_bb(&self, basic_block_h: BasicBlockHandle) -> &BasicBlock {
        self.basic_block_arena.get(basic_block_h).unwrap()
    }

    pub fn add_bb(&mut self, basic_block: BasicBlock) -> BasicBlockHandle {
        self.basic_block_arena.insert(basic_block)
    }

    pub fn get_const(&mut self, basic_block_h: BasicBlockHandle, value: i32) -> InstructionHandle {
        let inst = Instruction::Const(value);
        self.add_inst(basic_block_h, inst)
    }

    pub fn add_inst(&mut self, basic_block_h: BasicBlockHandle, inst: Instruction) -> InstructionHandle {
        let basic_block = self.basic_block_arena.get_mut(basic_block_h).unwrap();
        let h = self.instruction_arena.insert(inst);
        basic_block.instructions.push(h);
        h
    }

    pub fn add_to_entry(&mut self, inst: Instruction) -> InstructionHandle {
        let basic_block = self.basic_block_arena.get_mut(self.entry).unwrap();
        let h = self.instruction_arena.insert(inst);
        basic_block.instructions.push(h);
        h
    }
}

pub struct CFGPrintable<'ctx> {
    pub cfg: CFG,
    pub context: &'ctx Context<'ctx>,
    // This is disgusting:
    pub names: RefCell<SecondaryMap<InstructionHandle, i32>>,
    pub counter: RefCell<i32>,
}

impl <'ctx> CFGPrintable<'ctx> {
    pub fn print_bb(&self, basic_block_h: BasicBlockHandle) {
        let bb = self.cfg.basic_block_arena.get(basic_block_h).expect("basic block handle not valid");
        for inst in &bb.instructions {
            self.print_inst(inst);
        }
        
    }

    pub fn get_name(&self, inst: &InstructionHandle) -> i32 {
        // Does this involve a copy?
        let mut names = self.names.borrow_mut();
        let name = names.get(*inst);
        match name {
            Some(&val) => {
                val
            }
            None => {
                // Mutable RefMut ????? 
                let mut ctr = self.counter.borrow_mut();
                let tmp = ctr.clone();
                names.insert(*inst, *ctr);
                *ctr += 1;
                tmp
            }
        }
    }

    pub fn print_inst(&self, inst_h: &InstructionHandle) {

        let inst = self.cfg.instruction_arena.get(*inst_h).expect("instruction handle not valid");

        let inst_name = self.get_name(inst_h);
        let out = match inst {
            Instruction::Allocate(size) => {
                format!("%{} = Allocate %{}", inst_name, self.get_name(size))
            }
            Instruction::Load(location) => {
                match location {
                    MemoryLocation::Stack(allocate_inst, ..) => {
                        format!("%{} = Load Stack %{}", inst_name, self.get_name(allocate_inst))
                    }
                    MemoryLocation::DataSection(string) => {
                        format!("%{} = Load Data Section %{}", inst_name, self.context.resolve_string(*string))
                    }
                    MemoryLocation::Parameter(offset) => {
                        format!("%{} = Load Parameter {}", inst_name, offset)
                    }
                }
            }
            Instruction::LoadOffset(_, _) => todo!(),
            Instruction::Store(location, operand) => match location {
                MemoryLocation::Stack(allocate_inst, ..) => {
                    format!("Store Stack %{} <- {}", self.get_name(allocate_inst), self.get_name(operand))
                }
                MemoryLocation::DataSection(string) => {
                    format!("Store Data Section %{} <- {}", self.context.resolve_string(*string), self.get_name(operand))
                }
                MemoryLocation::Parameter(offset) => {
                    format!("Store Parameter {} <- {}", offset, self.get_name(operand))
                }
            }
            Instruction::StoreOffset(_, _, _) => todo!(),
            // BinaryOps:
            // Does having this as a separate type make it harder to match? 
            // Probably no becasue we have to do our own custom pattern matching anyways.
            Instruction::BinaryOp(optype, op1, op2) => {
                format!("%{} = {:?} %{} %{}", inst_name, optype, self.get_name(op1), self.get_name(op2))
            }
            Instruction::UnaryOp(_, _) => todo!(),
            
            // A.
            Instruction::Const(_) => todo!(),
            Instruction::Call(_) => todo!(),
            Instruction::Lea(_) => todo!(),
        };
        println!("{out}");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    instructions: Vec<InstructionHandle>,
    terminator: Option<Terminator>,
    incoming: Vec<BasicBlockHandle>
}

impl BasicBlock {
    fn validate(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Allocate(Operand), // How to make this one const, and the other one non-const?
    Lea(MemoryLocation), // This really doesn't belong here, but oh well!. 

    // Need some way to describe complex memory locations with an expression? 
    // Basically dumb things like *(&b + 10) = 4;
    // This should compile but we need to described *(&b + 10) as a 'memory location'

    Load(MemoryLocation),
    LoadOffset(MemoryLocation, Operand),

    Store(MemoryLocation, Operand),
    StoreOffset(Operand, MemoryLocation, Operand),

    BinaryOp(BinaryOpType, Operand, Operand), // Include Comparisons.
    UnaryOp(UnaryOpType, Operand),

    // Subroutine(Instruction, Instruction),

    Const(i32),
    Call(Rc<RefCell<CFG>>), 
}

pub type Operand = InstructionHandle;

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryLocation {
    Stack(InstructionHandle, usize), // Store allocate instructon. Allocate instruction and size.
    Parameter(usize),
    Expr(InstructionHandle),
    // This makes no sense.
    //GlobalOffset(i32),
    
    DataSection(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Jump(BasicBlockHandle),
    CondJump(InstructionHandle, BasicBlockHandle, BasicBlockHandle),
    Return(InstructionHandle),
    // Call() TODO
}

