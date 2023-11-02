Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)

TODO:
- implement deep copy for context (keeping structure intact)
- context inheritance syntax
- varargs ?
- iterator functions ?
- foreach and iterator (returns unit or value)
- @get(@context,"x") -> alias to @getvar("x") ? same for set and insert ?
- builtin STDLIB (returns struct) ?
- use MFEL Error type where needed instead of Rust errors
- builtin::F should take &[Expression] and eval should use a function table
- names of function types: function, staticfn, closure ?  lambda for closure ? closed for staticfn ?

- ref and deref (by name ?)
- implement return continue break

- Option<Result<Expression>> vs Result<Option<..>> for builtin_var
