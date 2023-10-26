use std::io::{read_to_string, stdin};
use pest::{Parser, pratt_parser::PrattParser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "expr.pest"]
pub struct ExprParser;

fn main() {
    let source = read_to_string(stdin()).unwrap();
    let file = ExprParser::parse(Rule::file, &source)
        .expect("unsuccessful parse") // unwrap the parse result
        .next().unwrap(); // get and unwrap the `file` rule; never fails

    println!("{:#?}",file);

    let _pratt_parser: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(sub, Left))
            .op(Op::infix(mult, Left) | Op::infix(div, Left))
    };
}