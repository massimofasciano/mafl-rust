use std::io::{read_to_string, stdin};
use mfel::{parse_string_to_ast, types::Context};

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let ast = parse_string_to_ast(&source);
    println!("{ast:#?}");
    let mut ctx = Context::new();
    let result = ctx.eval(&ast);
    println!("{result:#?}");
}
