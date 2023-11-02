Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)

TODO:
- context inheritance syntax
- varargs ?
- general while/for loop
- foreach and iterator (returns unit or value)
- make builtin @id return an expression (a closure if functional) ?
- builtin STDLIB (returns struct) ?
- use MFEL Error type where needed instead of Rust errors
- builtin::F should take &[Expression] and eval should use a function table
- can builtins be variables also ? ex: @env
- should a closure be more general? ex: Closure(ctx, expression)
- builtins should be able to have reserved names: ex: @and

- ref and deref (by name ?)
- implement return continue break
