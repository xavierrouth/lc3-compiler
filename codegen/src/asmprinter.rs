use core::fmt;
use std::{path::PathBuf, fs::File};
use std::io::Write;

pub struct AsmPrinter {
    instructions: Vec<LC3Bundle>,
    data: Vec<LC3Bundle>,
}   

//TODO: Support 'builder' style api for these:

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
        writeln!(file, "; To simulate output, use https://wchargin.com/lc3web/                       ")?;
        writeln!(file, "; ---------------------------------------------------------------------------")?;

        writeln!(file, ".ORIG x3000")?;

        for inst in &self.instructions {
            writeln!(file, "{inst}")?;
        }

        writeln!(file, "")?;
        writeln!(file, "; ------ Data Section ------ ")?;

        for datum in &self.data {
            writeln!(file, "{datum}")?;
        }

        writeln!(file, "USER_STACK        .FILL xFDFF")?;
        writeln!(file, "RETURN_SLOT       .FILL xFDFF")?;

        writeln!(file, ".END")?;

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
    Br(bool, bool, bool, Label),
    Jmp(Register),
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
    Fill(i32),
    Stringz(String),
    Blkw(i32),
    Orig,
    End
}

impl fmt::Display for LC3Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Can do allignment here.
            LC3Directive::Fill(value) => write!(f, ".FILL #{:0>4}", value), // Need to pritn size in hex somehow
            LC3Directive::Stringz(value) => write!(f, ".FILL #\"{:0>4}\"", value),
            LC3Directive::Blkw(value) => write!(f, ".FILL #{:0>4}", value),
            LC3Directive::Orig => write!(f, ".ORIG"),
            LC3Directive::End => write!(f, ".END"),
        }
    }
}

pub enum LC3Bundle {
    HeaderLabel(Label, Option<String>), // String is comment
    Instruction(LC3Inst, Option<String>), // String is comment
    Directive(Option<Label>, LC3Directive, Option<String>), // String is comment
    SectionComment(String),
    Newline,
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
                    Some(comment) => {
                        let inst = format!("    {inst}");
                        write!(f, "{:40}; {comment}", inst)
                    },  
                    None => write!(f, "    {inst}"),
                }
            },
            LC3Bundle::Directive(label, directive, comment) => {
                let directive = format!("{directive}");
                match (label, comment) {
                    (None, None) => write!(f, "{directive}"),
                    (None, Some(comment)) => write!(f, "{directive} {:<50}{comment}", ";"),
                    (Some(label), None) => {
                        let label = format!("{label}");
                        write!(f, "{:17} {directive}", label) },
                    (Some(label), Some(comment)) => write!(f, "{label} {directive} {:<50};{comment}", ";"),
                }
            },
            LC3Bundle::SectionComment(comment) => write!(f, "; {comment}"),
            LC3Bundle::Newline => {write!(f, "")},
        }
    }
}

impl fmt::Display for LC3Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LC3Inst::Noop => write!(f, "\n"),
            LC3Inst::AddReg(dest, src1, src2) =>  write!(f, "ADD {dest}, {src1}, {src2}"),
            LC3Inst::AddImm(dest, src1, src2) => write!(f, "ADD {dest}, {src1}, {src2}"),
            LC3Inst::AndReg(dest, src1, src2) =>  write!(f, "AND {dest}, {src1}, {src2}"),
            LC3Inst::AndImm(dest, src1, src2) => write!(f, "AND {dest}, {src1}, {src2}"),
            LC3Inst::Br(n, z, p, label) => {
                match (n, z, p) {
                    (true, true, true) => write!(f,   "BR   {label}"), // Same as BR
                    (true, true, false) => write!(f,  "BRnz {label}"),
                    (true, false, true) => write!(f,  "BRnp {label}"),
                    (true, false, false) => write!(f, "BRn  {label}"),
                    (false, true, true) => write!(f,  "BRzp {label}"),
                    (false, true, false) => write!(f, "BRz  {label}"),
                    (false, false, true) => write!(f, "BRp  {label}"),
                    (false, false, false) => write!(f, "; bad BR opcode"),
                }
            }
            LC3Inst::Jmp(reg) => write!(f, "JMP {reg}"), // This is same as ret actually
            LC3Inst::Jsr(label) => write!(f, "JSR {label}"),
            LC3Inst::Jsrr(_) => todo!(),
            LC3Inst::Ld(reg, label) => write!(f, "LD  {reg}, {label}"),
            LC3Inst::Ldi(reg, label) => write!(f, "LDI {reg}, {label}"),
            LC3Inst::Ldr(arg1, arg2, arg3) => write!(f, "LDR {arg1}, {arg2}, {arg3}"),
            LC3Inst::Lea(reg, label) => write!(f, "LEA {reg}, {label}"),
            LC3Inst::Not(dst, src) => write!(f, "NOT {dst}, {src}"),
            LC3Inst::Ret => write!(f, "RET"), // This is a special case of jump
            LC3Inst::Rti => todo!(),
            LC3Inst::St(reg, label) => write!(f, "ST  {reg}, {label}"),
            LC3Inst::Sti(reg, label) => write!(f, "STI {reg}, {label}"),
            LC3Inst::Str(arg1, arg2, arg3) => write!(f, "STR {arg1}, {arg2}, {arg3}"),
            LC3Inst::Trap(_) => todo!(),
            LC3Inst::Halt => write!(f, "HALT")
        }
        
    }
}