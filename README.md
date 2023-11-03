Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)

TODO:
- implement deep copy for context (keeping structure intact OR compacting)
- context inheritance syntax
- use statement or @use builtin to import contexts (ex: @use(@std.iter)). maybe @import ?
- use MFEL Error type where needed instead of Rust errors
- names of function types: function, staticfn, closure ?  lambda for closure ? closed for staticfn ?
- ref and deref (by name ?)
- implement return continue break
- Option<Result<Expression>> vs Result<Option<..>> for builtin_var
- struct syntax useful or not ? we have module.
