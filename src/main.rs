use std::io::{read_to_string, stdin};
use mfe_lang::{parse_string_to_ast, eval::eval, types::Context};

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let ast = parse_string_to_ast(&source);
    println!("{ast:#?}");
    let (ctx, result) = eval(Context::new(),&ast);
    println!("{ctx:#?}");
    println!("{result:#?}");
}
