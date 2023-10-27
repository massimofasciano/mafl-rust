use std::io::{read_to_string, stdin};
use test_pest_expr::{parse_string_to_ast, eval::{eval, Context}};

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let ast = parse_string_to_ast(&source);
    println!("{ast:#?}");
    let val = eval(Context::new(),&ast);
    println!("{val:?}");
}
