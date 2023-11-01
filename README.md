Rust interpreter for the MFEL language

TODO:
- there is a loop somewhere (circular expressions are sometimes created, stack overflows...)
when function becomes closure, it keeps a Rc to the parent context
when it is bound to a variable in the outside ctx, that binding appears inside the shared ctx too (pointers to same ctx)
that creates a circular ref
it's also undesirable semantically (changing the closure from outside)
the problem happens with "var" inside a sequence/block but not with "let in" chains because they create a new scope at every level

- closures as objects (field notation accesses context bindings)  EXPERIMENTAL (o.x works, not o.x.y on assign) GET RID OF THIS

- array @map @fold ?
- store arrays in RefCell to allow mutation of elements ?
- context objects (lists of hashes exposing the Context type to the language): reuse the field syntax ? use builtin functions ?

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
