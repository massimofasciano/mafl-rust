use std::io::{read_to_string, stdin};
use mfel::{parse_source, eval::eval, context::Context};

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let ast = parse_source(&source);
    // println!("{ast:#?}");
    let mut ctx = Context::new();
    let result = eval(&mut ctx, &ast);
    // println!("{result:#?}");
    println!("{result}");
}
