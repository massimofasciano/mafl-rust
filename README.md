Rust interpreter for the MFEL language

TODO:
- deal with errors
- Value and Ast should be different types
- ref and deref (by name ?)
- Rc instead of Box ?
- rename Ast (Syntax) and Context (Evaluator) ?
- lambda/closure (rename?) vs function/func/fn (no env capture)
- test recursion
- builtins: $readln $readfile $include $add $sub $arg_str $arg_val
- change unit literal from () to ?  (nil, none, void, null, etc...) 
- implement return continue break
- general while/for loop
- foreach and iterator (returns unit or value)
- currying on builtins ?
- closures as objects (field notation accesses context bindings)
- let f = function (x,y,z) in (g1,g2) { ... g1+g2}  captures g1 and g2 only in closure
- let obj = context { var field1 = 1; var field2 = 2; }  and then obj.field1, obj.field2 (works on function closures too)
- context { ... } is equiv to function () in () { ... }
- arrays and varargs
- how to mutate a binding (for arrays)
- explain why pairs instead of hashmap for bindings