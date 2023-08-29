use std::{path::PathBuf, fs::File, io::Read, cell::RefCell, rc::Rc};
use clap::{Parser, error};
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

    let root = parser.parse_translation_unit();
    // Check errors here
    if let Err(error) = root {
        error_handler.borrow_mut().print_parser_error(error);
        return;
    }

    let root = root.unwrap();

    if cli.verbose {
        let mut printer: ASTPrint = ASTPrint::new(false, &parser.ast);
        printer.traverse(&root);
    }

    let mut analyzer: Analyzer<'_> = analysis::Analyzer::new(&parser.ast);
    
    // Check for analysis errors
    analyzer.traverse(&root);
    analyzer.print_symbol_table();


    //println!("two: {:?}", cli.verbose);

}
