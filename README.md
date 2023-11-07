Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)

TODO:

- BIG BUG!!!! global variables appear to override locals in function calls!!!

- class statement ?
- module as an expression (like before) ? call it object ?

- String as array of characters ? mutable.

- ref and deref: refs as objects, what syntax to deref on assign ?
&x creates a Ref
p^ or *p derefs
p^ = 10 or p ^= 10 to deref assign

- *=, +=, etc...

- @split, @regex, @command

- builtin in grammar

- block / function block / loop block / if block (for continue break return)

- propagate anyhow errors into the language as Error types ?

- offer both shallow and deep copy ?

- context inheritance syntax
- use statement or @use builtin to import contexts (ex: @use(@std.iter)). maybe @import ?
- use MFEL Error type where needed instead of Rust errors

- struct syntax useful or not ? we have module.

- mutual recursion (how to bind) ?






