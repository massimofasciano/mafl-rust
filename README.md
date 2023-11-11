Rust parser and interpreter for the MFEL language

FEATURES:
- added internal stdlib (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)
- object factory

TODO:

- try a tree structure for contexts
- walker for expression and context with cycle detection

- clean up the expression type (atoms, operators, code, input, etc...)

- weak references ? gc ?

- lookup table for builtins ?

- ref and deref: refs as objects, what syntax to deref on assign ?
&x creates a Ref
p^ or *p derefs
p^ = 10 or p ^= 10 to deref assign

- *=, +=, etc...

- context inheritance syntax
- use statement or @use builtin to import contexts (ex: @use(@std.iter)). maybe @import ?







