Rust interpreter for the MFEL language

FIXED:
- added capture method to Context to fix circular references in closures

FEATURES:
- added function (no capture) vs closure (captures context)
- added stdlib (coded in mfel)

TODO:
- dynamic(x) {x+y}: evaluated in dynamic ctx 
- builtin STDLIB (returns struct) ?
- test recursion
- closures as objects (field notation accesses context bindings)  EXPERIMENTAL (o.x works, not o.x.y on assign) GET RID OF THIS

- STDLIB! (was: array @map @fold ?)
- store arrays in RefCell to allow mutation of elements ?
- context objects (lists of hashes exposing the Context type to the language): reuse the field syntax ? use builtin functions ?

- ref and deref (by name ?)
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
