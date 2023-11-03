Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)

TODO:
- propagate anyhow errors into the language as Error types ?

- implement deep copy for closure

- context inheritance syntax
- use statement or @use builtin to import contexts (ex: @use(@std.iter)). maybe @import ?
- use MFEL Error type where needed instead of Rust errors

- names of function types: function, staticfn, closure ?  function -> dynfn, closure -> lambda, staticfn (remove and use context () in lambda ...)

- ref and deref (by name ?)
- implement return continue break
- Option<Result<Expression>> vs Result<Option<..>> for builtin_var
- struct syntax useful or not ? we have module.
- ast: Expression or ast: &Expression in eval ?

- thunk statement (same as closure but without need to call). maybe call it lazy ?

- parallel rec or patch closures after binding (with @bind)
- extend field notation for let and assign ? m.x := 5
- extend array notation for assign ? a[3] := 5

- f(a,*b,c) expands to f(a,b1,b2,...,bn,c) ? identifier prefix for calls

- implement xor_nn in MFEL


