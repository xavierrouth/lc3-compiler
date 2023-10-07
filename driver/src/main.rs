#![deny(rust_2018_idioms)]
use std::{path::PathBuf, fs::File, io::Read};
use analysis::{symres::SymbolResolutionPass, typecheck::{TypecheckPass}, typedast::{TypedASTPrint, TypedVistior}};
use clap::{Parser};
use intermediate::{hirgen::{self, HIRGen}, hir::HIR};
//use codegen::asmprinter::AsmPrinter;
use lex_parse::{lexer, parser, ast::{ASTPrint, Vistior}, error::ErrorHandler, context::Context};

#[derive(Parser, Debug)]
#[command(name = "LC3-Compiler")]
#[command(about = "A C to LC3 Compiler built for students at the University of Illinois, \nUrbana-Champaign by HKN (https://hkn-alpha.netlify.app).\nReport bugs to <xrouth2@illinois.edu>.", long_about = None)]
#[command(author, version)] // Read from `Cargo.toml`
struct Cli {
    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output: Option<PathBuf>,

    // Produce verbose output (print AST)
    #[arg(short = 'v', long = "verbose", default_value_t = false)]
    verbose: bool, 

    // Disables most analysis errors (type checking)
    #[arg(long = "sandbox", default_value_t = false)]
    sandbox: bool,

    // Output path
    #[arg(short = 'S', long = "asm", default_value_t = false)]
    asm: bool, 
    
    // Enable debugging information
    #[arg(short = 'g', long = "debug", default_value_t = false)]
    debug: bool,

    #[arg(value_name = "INPUT_FILE")]
    input: PathBuf,

}

fn main() {

    let cli = Cli::parse();

    let input_path: PathBuf  = cli.input;

    let mut input_file = match File::open(input_path.clone()) {
        Ok(input_file) => input_file,
        Err(_) => {println!("Invalid input file. {:?}", input_path); return},
    };

    let mut input_stream = String::new();

    input_file.read_to_string(&mut input_stream).unwrap();

    let context = Context::new(&input_stream);
    let error_handler: ErrorHandler<'_> = ErrorHandler::new(&context);

    let mut lexer: lexer::Lexer<'_, '_> = lexer::Lexer::new(&input_stream, &context, &error_handler);
    let parser: parser::Parser<'_> = parser::Parser::new(&mut lexer, &context, &error_handler);

    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(error) => {error_handler.print_parser_error(error); return},
    };

    let root = &ast.root.unwrap();
    if cli.verbose {
        let mut printer: ASTPrint<'_> = ASTPrint::new(false, &ast, &context);
        printer.traverse(*root);
    }

    let mut analyzer: SymbolResolutionPass<'_> = SymbolResolutionPass::new(&ast, &context, &error_handler);
    
    // Check for analysis errors
    analyzer.traverse(*root);

    if *error_handler.fatal.borrow() {
        return;
    }

    if cli.verbose {
        // analyzer.print_symbol_table();
    } 

    // Should use some sort of AnalyzerResult interface instead of stealing members. 
    let mut symbtab = analyzer.symbol_table;
    let scopes = analyzer.scopes;

    let typecheck: TypecheckPass<'_> = TypecheckPass::new(ast, &mut symbtab, &context, scopes, &error_handler);

    let ast = typecheck.run();


    if cli.verbose {
        let mut typed_printer = TypedASTPrint::new(false, &ast, &context);
        typed_printer.traverse(ast.root.expect("invalid root"));
    };

    let hirgen = HIRGen::new(ast, symbtab, &context, &error_handler);

    let hir: HIR<'_> = hirgen.run();

    hir.print();


    /* 
    let mut printer = codegen::asmprinter::AsmPrinter::new();
    let mut codegen = codegen::codegen::Codegen::new(&ast, &mut printer, &context, analyzer.symbol_table, casts);

    codegen.emit_ast_node(&ast.root.unwrap());

    let outfile = match cli.output {
        Some(file) => file,
        None => PathBuf::from(r"/out.asm")
    };
    printer.print_to_file(outfile); */

    //println!("two: {:?}", cli.verbose);  */

}

/*
#[cfg(test)]
mod binary_tests {
    use std::process::Command;
    use std::fs;


    #[test]
    fn basic() {
        let paths = fs::read_dir("../tests/").unwrap();
        for path in paths {
            println!("Name: {}", path.unwrap().path().display())
        }

        /*let compile = Command::new("lc3-compile")
            .arg("in/main.c")
            .arg("-o")
            .arg("out/out.asm")
            .output()
            .unwrap_or_else(|e| {panic!("failed to compile: {e}")});
        */
        //let test = Command::new(lc3)
        
    }
    
} */