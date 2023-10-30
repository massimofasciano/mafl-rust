Rust interpreter for the MFEL language

TODO:
- ref and deref (by name ?)
- Value and Ast should be different types
- Rc instead of Box ?
- rename Ast (Syntax) and Context (Evaluator) ?
- lambda/closure (rename?) vs function/func/fn (no env capture)
- test recursion
- builtins: $readln $readfile $include $add $sub $arg_str $arg_val
- pipe infix op |
- remove unit literal () or make it {}
- implement return continue break
- general while/for loop
- foreach and iterator (returns unit or value)
