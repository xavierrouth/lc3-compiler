#![deny(rust_2018_idioms)]
use std::{path::PathBuf, fs::File, io::Read, cell::RefCell, rc::Rc};
use analysis::{analysis::Analyzer, typecheck::Typecheck};
use clap::{Parser, error};
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

    let root =&ast.root.unwrap();
    if cli.verbose {
        let mut printer: ASTPrint<'_> = ASTPrint::new(false, &ast, &context);
        printer.traverse(root);
    }

    
    let mut analyzer: Analyzer<'_> = Analyzer::new(&ast, &context, &error_handler);
    
    // Check for analysis errors
    analyzer.traverse(root);

    if cli.verbose {
        analyzer.print_symbol_table();
    } 

    let mut typecheck: Typecheck<'_> = Typecheck::new(&analyzer.symbol_table, &ast, &context, &error_handler);

    typecheck.traverse(root);

    typecheck.print_casts();

    /*

    let mut printer = codegen::asmprinter::AsmPrinter::new();
    let mut codegen = codegen::codegen::Codegen::new(&ast, &mut printer, analyzer.symbol_table);

    codegen.emit_ast_node(&ast.root.unwrap());

    let outfile = match cli.output {
        Some(file) => file,
        None => PathBuf::from(r"/out.asm")
    };
    printer.print_to_file(outfile);

    //println!("two: {:?}", cli.verbose);  */

}
