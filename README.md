Rust interpreter for the MFEL language

TODO:
- closures as objects (field notation accesses context bindings)  EXPERIMENTAL (o.x works, not o.x.y on assign)
- using Rc+RefCell on closure context for field mutation. should extend to other parts of expression and context ?

- ref and deref (by name ?)
- lambda/closure (rename?) vs function/func/fn (no env capture)
- test recursion
- builtins: $readln $readfile $include $add $sub $arg_str $arg_val
- change unit literal from () to ?  (nil, none, void, null, etc...) 
- implement return continue break
- general while/for loop
- foreach and iterator (returns unit or value)
- currying on builtins ?
- let f = function (x,y,z) in (g1,g2) { ... g1+g2}  captures g1 and g2 only in closure
- let obj = context { var field1 = 1; var field2 = 2; }  and then obj.field1, obj.field2 (works on function closures too)
- context { ... } is equiv to function () in () { ... }
- arrays and varargs
