


use std::{cell::{RefCell, Cell}, rc::Rc, fmt::{Display, write}, alloc::Layout, collections::HashMap};

use crate::TypedArena;

use analysis::{symtab::{VarDecl}};
use lex_parse::{ast::{ASTNodeHandle, AST}, error::ErrorHandler, context::{InternedString, Context, InternedType}, types::TypePrintable};
use slotmap::{SparseSecondaryMap, SlotMap, SecondaryMap};

// TODO: Should be consistent in naming of type data vs type handle. Which one gets the original name, which one gets the suffix?

slotmap::new_key_type! { pub struct InstructionHandle; }
slotmap::new_key_type! { pub struct BasicBlockHandle; }

/* High level IR, generated from AST, moved to SSA form, DCE,  */
pub struct HIR<'ctx> {
    pub main: Option<i32>,
    pub functions: Vec<Rc<RefCell<CFG<'ctx>>>>,
    pub data: Vec<VarDecl>,
    pub context: &'ctx Context<'ctx>,
}

impl <'ctx> HIR<'ctx> {
    pub fn print(& self) -> () {
        for func in &self.functions {
            let cfg = func.borrow().clone();
            let printable = CFGPrintable::new(cfg);
            printable.print();
        }
    }
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

/* =============== Control FLow Graph ============ */
/* CFG for a single function */
#[derive(Debug, Clone)]
pub struct CFG<'ctx> {
    pub(crate) entry: BasicBlockHandle,
    pub(crate) name: InternedString,
    pub(crate) instruction_arena: SlotMap<InstructionHandle, Instruction<'ctx>>,
    pub(crate) basic_block_arena: SlotMap<BasicBlockHandle, BasicBlock>,

    pub basic_block_order: Vec<BasicBlockHandle>,

    return_ty: InternedType, 

    // Stack Frame:
    parameters_offset: usize,
    locals_offset: usize,
    locals: HashMap<VarDecl, MemoryLocation>, 
    // Globals??
    // Ref to 'globals' hashmap?

    pub context: &'ctx Context<'ctx>,
}

#[derive(Debug, Clone)]
struct LocalVarSlot {
    size: i32,
}

impl PartialEq for CFG<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.entry == other.entry && self.name == other.name
    }
}


impl <'ctx> CFG<'ctx> {
    pub fn new(name: InternedString, return_ty: InternedType, context: &'ctx Context<'ctx>,) -> CFG<'ctx> {
        let mut basic_block_arena = SlotMap::with_key();
        let mut basic_block_order = Vec::new();
        let entry_name = context.get_string("entry");
        let entry = basic_block_arena.insert(BasicBlock { name: entry_name, instructions: Vec::new(), terminator: None, incoming: Vec::new() });
        basic_block_order.push(entry);
        CFG {
            context,
            return_ty,
            instruction_arena: SlotMap::with_key(),
            basic_block_arena,
            basic_block_order,
            name,
            entry,
            parameters_offset: 0,
            locals_offset: 0,
            locals: HashMap::new(),
        }
    }

    pub fn new_bb(&mut self, name: InternedString) -> BasicBlockHandle {
        let bb = BasicBlock { name, instructions: Vec::new(), terminator: None, incoming: Vec::new() };
        
        let h = self.basic_block_arena.insert(bb);
        self.basic_block_order.push(h.clone());
        h
    }

    pub fn get_location(&self, local: VarDecl) -> &MemoryLocation {
        match self.locals.get(&local) {
            Some(local) => local,
            None => panic!("variable does not belong to current function or hasn't been allocated yet.")
        }
    }

    pub fn add_parameter(&mut self, parameter: VarDecl) -> () {
        let size = parameter.size;
        self.locals.entry(parameter).or_insert(MemoryLocation::Parameter(self.parameters_offset));
        self.parameters_offset += size;
    }

    pub fn add_local(&mut self, local: VarDecl, alloca: InstructionHandle) -> () { // Do we need scope information here? (scope: ScopeHandle)
        let size = local.size;
        self.locals.entry(local).or_insert(MemoryLocation::Stack(alloca, self.locals_offset));
        self.locals_offset += size;
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

    pub fn add_inst(&mut self, basic_block_h: BasicBlockHandle, inst: Instruction<'ctx>) -> InstructionHandle {
        let basic_block = self.basic_block_arena.get_mut(basic_block_h).unwrap();
        let h = self.instruction_arena.insert(inst);
        basic_block.instructions.push(h);
        h
    }

    pub fn add_to_entry(&mut self, inst: Instruction<'ctx>) -> InstructionHandle {
        let basic_block = self.basic_block_arena.get_mut(self.entry).unwrap();
        let h = self.instruction_arena.insert(inst);
        basic_block.instructions.push(h);
        h
    }
}

pub struct CFGPrintable<'ctx> {
    pub cfg: CFG<'ctx>,
    // This is disgusting:
    pub names: RefCell<SecondaryMap<InstructionHandle, i32>>,
    pub counter: RefCell<i32>,
}

impl <'ctx> CFGPrintable<'ctx> {
    pub fn new(cfg: CFG<'ctx>) -> CFGPrintable<'ctx> {
        CFGPrintable { cfg,
            names: RefCell::new(SecondaryMap::new()), 
            counter: RefCell::new(0) }
    }

    pub fn print(&self) {
        let return_type = TypePrintable { context: self.cfg.context, data: self.cfg.context.resolve_type(&self.cfg.return_ty) };
        println!("fn @{}() -> {return_type}:", self.cfg.context.resolve_string(self.cfg.name));
        // We really should maintain an ordering of basic blocks in the cfg.
        for bb in self.cfg.basic_block_arena.keys() {
            self.print_bb(bb);
            println!("test");
        }
    }

    pub fn print_bb(&self, basic_block_h: BasicBlockHandle) {
        let bb = self.cfg.basic_block_arena.get(basic_block_h).expect("basic block handle not valid");
        let name = self.cfg.context.resolve_string(bb.name);
        println!("{name}:");
        for inst in &bb.instructions {
            self.print_inst(inst);
        }
        
    }

    pub fn get_inst_name(&self, inst: &InstructionHandle) -> i32 {
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

    pub fn get_bb_name(&self, bb_h: &BasicBlockHandle) -> String {
        let bb = self.cfg.resolve_bb(*bb_h);
        let name = bb.name;
        self.cfg.context.resolve_string(name)
    }

    pub fn print_inst(&self, inst_h: &InstructionHandle) {
        let inst = self.cfg.instruction_arena.get(*inst_h).expect("instruction handle not valid");

        let inst_name = self.get_inst_name(inst_h);
        let out = match inst {
            Instruction::Allocate(size) => {
                format!("%{} = Allocate %{}", inst_name, self.get_inst_name(size))
            }
            Instruction::Load(location) => {
                match location {
                    MemoryLocation::Stack(allocate_inst, ..) => {
                        format!("%{} = Load Stack %{}", inst_name, self.get_inst_name(allocate_inst))
                    }
                    MemoryLocation::DataSection(string) => {
                        format!("%{} = Load Data Section %{}", inst_name, self.cfg.context.resolve_string(*string))
                    }
                    MemoryLocation::Parameter(offset) => {
                        format!("%{} = Load Parameter {}", inst_name, offset)
                    }
                    MemoryLocation::Expr(inst) => {
                        format!("%{} = Load Expr %{}", inst_name, self.get_inst_name(inst))
                    }
                }
            }
            Instruction::LoadOffset(_, _) => todo!(),
            Instruction::Store(location, operand) => match location {
                MemoryLocation::Stack(allocate_inst, ..) => {
                    format!("Store Stack %{} <- %{}", self.get_inst_name(allocate_inst), self.get_inst_name(operand))
                }
                MemoryLocation::DataSection(string) => {
                    format!("Store Data Section %{} <- %{}", self.cfg.context.resolve_string(*string), self.get_inst_name(operand))
                }
                MemoryLocation::Parameter(offset) => {
                    format!("Store Parameter %{} <- %{}", offset, self.get_inst_name(operand))
                }
                MemoryLocation::Expr(inst) => {
                    format!("%{} = Store Expr %{}", inst_name, self.get_inst_name(inst))
                }
            }
            Instruction::StoreOffset(_, _, _) => todo!(),
            // BinaryOps:
            // Does having this as a separate type make it harder to match? 
            // Probably no becasue we have to do our own custom pattern matching anyways.
            Instruction::BinaryOp(optype, op1, op2) => {
                format!("%{} = {:?} %{} %{}", inst_name, optype, self.get_inst_name(op1), self.get_inst_name(op2))
            }
            Instruction::UnaryOp(_, _) => todo!(),
            
            // A.
            Instruction::Const(val) => {
                format!("%{} = {val}", inst_name)
            }
            Instruction::Call(_) => todo!(),
            Instruction::Lea(location) => {
                format!("%{} = Lea {:?}", inst_name, location)
            }
            Instruction::Return(op) => {
                format!("return %{}", self.get_inst_name(op))
            }
            Instruction::CondBr(cond, true_bb, false_bb) => {
                format!("Br %{}, {}, {}", self.get_inst_name(cond), self.get_bb_name(true_bb), self.get_bb_name(false_bb))
            }
            Instruction::Br(bb_h) => {
                format!("Br %{}", self.get_bb_name(bb_h))
            }
        };
        println!("{out}");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    name: InternedString,
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
pub enum Instruction<'ctx> {
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

    CondBr(Operand, BasicBlockHandle, BasicBlockHandle), // Condition, true, false,
    Br(BasicBlockHandle),
    
    // Subroutine(Instruction, Instruction),
    Return(Operand),
    Const(i32),
    Call(Rc<RefCell<CFG<'ctx>>>), 
}

pub type Operand = InstructionHandle;

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryLocation {
    Stack(InstructionHandle, usize), // Store allocate instructon. Allocate instruction and size.
    Parameter(usize),
    Expr(InstructionHandle), // This is so annoying! 
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

