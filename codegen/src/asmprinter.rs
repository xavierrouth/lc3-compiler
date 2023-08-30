use core::fmt;
use std::{path::PathBuf, fs::File};
use std::io::Write;

pub struct AsmPrinter {
    instructions: Vec<LC3Bundle>,
    data: Vec<LC3Bundle>,
}   

impl AsmPrinter {
    pub fn new() -> AsmPrinter {
        AsmPrinter { instructions: Vec::new(), data: Vec::new() }
    }
    pub fn inst(&mut self, inst: LC3Bundle) -> () {
        self.instructions.push(inst);
    }

    pub fn data(&mut self, data: LC3Bundle) -> () {
        self.data.push(data);
    }   

    pub fn print_to_file(&mut self, path: PathBuf) -> std::io::Result<()> {

        let mut file = File::create(path)?;

        // Temporary print prelude
        writeln!(file, "; ---------------------------------------------------------------------------")?;
        writeln!(file, "; C-LC3 Compiler, by HKN for UIUC Students                                   ")?;
        writeln!(file, "; Disclaimer: Not all C features are supported by this compiler. Do not base ")?;
        writeln!(file, "; assumptions about valid C programming on what you see here.                ")?;
        writeln!(file, "; Please report any bugs or unexpected crashes to <xrouth2@illinois.edu>     ")?;
        writeln!(file, "; ---------------------------------------------------------------------------")?;

        for inst in &self.instructions {
            writeln!(file, "{inst}")?;
        }

        writeln!(file, "; ------ Data Section ------ ")?;

        for datum in &self.data {
            writeln!(file, "{datum}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Label {
    Label(String)
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Label::Label(string) => write!(f, "{}", string)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register {
    pub value: usize
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "R{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Immediate {
    Hex(i32),
    Int(i32)
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Immediate::Hex(val) => write!(f, "{:#06X}", val),
            Immediate::Int(val) => write!(f, "#{}", val),
        }
    }
}

pub enum LC3Inst {
    Noop,
    AddReg(Register, Register, Register),
    AddImm(Register, Register, Immediate),
    AndReg(Register, Register, Register),
    AndImm(Register, Register, Immediate),
    Br(bool, bool, bool),
    Jmp(Label),
    Jsr(Label),
    Jsrr(Register),
    Ld(Register, Label), // Should be Label or immediate, but almost always Label
    Ldi(Register, Label),
    Ldr(Register, Register, Immediate),
    Lea(Register, Label),
    Not(Register, Register),
    Ret,
    Rti, // What?
    St(Register, Label),
    Sti(Register, Label),
    Str(Register, Register, Immediate),
    Trap(String), // Trap vector.
    Halt // We should really sub-enum trap above ^^^
}

pub enum LC3Directive {
    Fill(usize),
    Stringz(String),
    Blkw(usize),
    Orig,
    End
}

impl fmt::Display for LC3Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Can do allignment here.
            LC3Directive::Fill(value) => write!(f, ".FILL {:>20}", value), // Need to pritn size in hex somehow
            LC3Directive::Stringz(value) => write!(f, ".FILL \"{:>20}\"", value),
            LC3Directive::Blkw(value) => write!(f, ".FILL {:>20}", value),
            LC3Directive::Orig => write!(f, ".ORIG"),
            LC3Directive::End => write!(f, ".END"),
        }
    }
}

pub enum LC3Bundle {
    HeaderLabel(Label, Option<String>), // String is comment
    Instruction(LC3Inst, Option<String>), // String is comment
    Directive(Option<Label>, LC3Directive, Option<String>) // String is comment
}

impl fmt::Display for LC3Bundle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LC3Bundle::HeaderLabel(label, comment) => {
                match comment {
                    Some(comment) => write!(f, "{label}, ; {comment}"),
                    None => write!(f, "{label}"),
                }
            }
            LC3Bundle::Instruction(inst, comment) => {
                match comment { // Note indentation here:
                    Some(comment) => write!(f, "{}{inst}, {:>30} {comment}", "    ", ";"),
                    None => write!(f, "    {inst}"),
                }
            },
            LC3Bundle::Directive(label, directive, comment) => {
                match (label, comment) {
                    (None, None) => write!(f, "{directive}"),
                    (None, Some(comment)) => write!(f, "{directive} {:>30} {comment}", ";"),
                    (Some(label), None) => write!(f, "{label} {directive}"),
                    (Some(label), Some(comment)) => write!(f, "{label} {directive} {:>30} {comment}", ";"),
                }
            },
        }
    }
}

impl fmt::Display for LC3Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LC3Inst::Noop => write!(f, "\n"),
            LC3Inst::AddReg(dest, src1, src2) => write!(f, "ADD {dest}, {src1}, {src2}"),
            LC3Inst::AddImm(dest, src1, src2) => write!(f, "ADD {dest}, {src1}, {src2}"),
            LC3Inst::AndReg(dest, src1, src2) => write!(f, "AND {dest}, {src1}, {src2}"),
            LC3Inst::AndImm(dest, src1, src2) => write!(f, "AND {dest}, {src1}, {src2}"),
            LC3Inst::Br(n, z, p) => {
                match (n, z, p) {
                    (true, true, true) => write!(f, "BR  "), // Same as BR
                    (true, true, false) => write!(f, "BRnz"),
                    (true, false, true) => write!(f, "BRnp"),
                    (true, false, false) => write!(f, "BRn "),
                    (false, true, true) => write!(f, "BRzp"),
                    (false, true, false) => write!(f, "BRz "),
                    (false, false, true) => write!(f, "BRp "),
                    (false, false, false) => write!(f, "; bad BR opcode"),
                }
            }
            LC3Inst::Jmp(_) => todo!(), // This is same as ret actually
            LC3Inst::Jsr(label) => write!(f, "JSR {label}"),
            LC3Inst::Jsrr(_) => todo!(),
            LC3Inst::Ld(reg, label) => write!(f, "LD  {reg}, {label}"),
            LC3Inst::Ldi(reg, label) => write!(f, "LDI {reg}, {label}"),
            LC3Inst::Ldr(arg1, arg2, arg3) => write!(f, "LDR {arg1}, {arg2}, {arg3}"),
            LC3Inst::Lea(_, _) => todo!(),
            LC3Inst::Not(dst, src) => write!(f, "NOT {dst}, {src}"),
            LC3Inst::Ret => write!(f, "RET"),
            LC3Inst::Rti => todo!(),
            LC3Inst::St(reg, label) => write!(f, "ST  {reg}, {label}"),
            LC3Inst::Sti(reg, label) => write!(f, "STI {reg}, {label}"),
            LC3Inst::Str(arg1, arg2, arg3) => write!(f, "STR {arg1}, {arg2}, {arg3}"),
            LC3Inst::Trap(_) => todo!(),
            LC3Inst::Halt => write!(f, "HALT")
        }
        
    }
}