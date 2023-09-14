# C-LC3-Compiler

C-LC3-Compiler is a modern, student built, C compiler targeting the LC3 Assembly language as described in *Introduction to Computing* by Dr. Yale Patt and Dr. Sanjay Patel. This tool is mainly for educational purposes, and is specifically meant to help students taking ECE 220 - Computer Systems & Programming at UIUC, however it should be relevant and useful to any student learning LC3 Assembly. The calling conventions implemented by the compiler mirror the conventions described in *Introduction to Computing*, any discrepancies are bugs.

This repository contains the source code for the compiler, as well as various tests. Currently, only a subset of the C language is supported. Some important features I am working on include:

- String literals
- C operator subroutines (multiplication, division, etc.)
- I/O library functions
- Arrays
- Structs and Unions
- Typedefs
- Function Pointers
- C Preprocessor

Some compiler explorer integration features I am working on include:

- Assembly documentation
- Instruction highlighting in source
- Code execution

### Build Instructions
To build the compiler locally,
1. Ensure you have Cargo installed. 
2. `git clone https://github.com/xavierrouth/lc3-compiler.git`
3. `cd lc3-compiler`
4. `cargo install --path driver`

To use the compiler without building from source I recommend using https://godbolt.org/.

### Usage Gudie
To run the compiler, use `lc3-compile`:

```
A C to LC3 Compiler built for students at the University of Illinois Urbana-Champaign by HKN (https://hkn-alpha.netlify.app).

Usage: lc3-compile [OPTIONS] <INPUT_FILE>

Arguments:
  <INPUT_FILE>  

Options:
  -o, --output <FILE>  
  -v, --verbose        
  -g, --debug          
  -h, --help           Print help
  -V, --version        Print version
```

To run the tests, run the `test` executable from the build directory.

### LC3Tools
This project uses the [LC3Tools](https://github.com/chiragsakhuja/lc3tools) repository in order to assemble and verify the assembly generated from the compiler. Students working with LC3 may find tools from there helpful as well.

### Credits
The C-LC3-Compiler is developed and maintained by students from [HKN-Alpha](https://hkn-alpha.netlify.app/) at UIUC.

### Contributing and Bug Reporting
If you find a bug, please notify \<xrouth2 at illinois dot edu\>. We are always open to contributions, feel free to submit PRs or open discussion about what can be improved.
