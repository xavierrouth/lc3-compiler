#![deny(rust_2018_idioms)]
use std::{path::PathBuf, fs::File, io::Read, cell::RefCell, rc::Rc};
use clap::{Parser, error};
use codegen::asmprinter::AsmPrinter;
use lex_parse::{lexer, parser, ast::{ASTPrint, Vistior}, analysis::{Analyzer, self}, error::ErrorHandler};

#[derive(Parser, Debug)]
#[command(name = "LC3-Compiler")]
#[command(about = "A C to LC3 Compiler built for students at the University of Illinois Urbana-Champaign by HKN (https://hkn-alpha.netlify.app).", long_about = None)]
#[command(author, version)] // Read from `Cargo.toml`
struct Cli {
    #[arg(short = 'o', long = "option", value_name = "FILE")]
    output: Option<PathBuf>,

    #[arg(short = 'v', long = "verbose", default_value_t = false)]
    verbose: bool, 
    
    #[arg(short, long, default_value_t = false)]
    debug: bool,

    #[arg(value_name = "INPUT_FILE")]
    input: PathBuf,

}

fn main() {

    let cli = Cli::parse();

    let input_path: PathBuf  = cli.input;

    let mut input_file = File::open(input_path).unwrap();

    let mut input_stream = String::new();

    input_file.read_to_string(&mut input_stream).unwrap();

    let error_handler = Rc::new(RefCell::new(ErrorHandler::new()));

    let mut lexer: lexer::Lexer<'_> = lexer::Lexer::new(&input_stream, error_handler.clone());
    let mut parser: parser::Parser<'_> = parser::Parser::new(&mut lexer, error_handler.clone());

    let ast = parser.parse();

    if ast.is_none() {
        return;
    }

    let ast = ast.unwrap();

    if cli.verbose {
        let mut printer: ASTPrint<'_> = ASTPrint::new(false, &ast);
        printer.traverse(&ast.root.unwrap());
    }

    let mut analyzer: Analyzer<'_> = analysis::Analyzer::new(&ast, error_handler.clone());
    
    // Check for analysis errors
    analyzer.traverse(&ast.root.unwrap());
    analyzer.print_symbol_table();

    let mut printer = codegen::asmprinter::AsmPrinter::new();
    let mut codegen = codegen::codegen::Codegen::new(&ast, &mut printer, analyzer.symbol_table);

    codegen.emit_ast_node(&ast.root.unwrap());

    let outfile = match cli.output {
        Some(file) => file,
        None => PathBuf::from(r"/out.asm")
    };
    printer.print_to_file(outfile);

    //println!("two: {:?}", cli.verbose);

}
