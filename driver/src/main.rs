use std::{io, path::PathBuf};
use clap::Parser;

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
    input: Option<PathBuf>,

}

fn main() {

    let cli = Cli::parse();

    let input: Option<&PathBuf> = cli.input.as_ref();

    println!("{:?}", input);

    //println!("two: {:?}", cli.verbose);

}
