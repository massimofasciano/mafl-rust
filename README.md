Rust parser and interpreter for the MFEL language

FEATURES:
- added internal stdlib (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)
- object factory

TODO:

- don't forget to update open vars when adding new constructs!!! (like inheritance or with)  *****

- implement USE expr (to import all symbols from closure expr by ref flattened)

- constructor is fun that returns @self
- defcon defines constructor (useful ?)
- module is zero-arg constructor that is immediately called with zero args (creating an object)
- defmod defines module ?

- eliminate defun and all def... ? add letrec instead ?

- context should take ref
- replace context with object ?

- field (object) based builtins like .copy() .string() .map() ?

- curry @builtin and op (LUT for builtins with fixed arg list, op syntax maybe ?)
- builtins and ops should behave like lambdas

- try a tree structure for contexts: NO! a stack of stacks (list of lists) is a better analogy.
- walker for expression and context with cycle detection

- clean up the expression type (atoms, code, input, etc...)

- weak references ? gc ?

- lookup table for builtins ?
- put all builtins inside the interpreter struct

- context inheritance syntax (maybe context (a,b,c) <: parent {})
- use statement or @use builtin to import contexts (ex: @use(@std.iter)). maybe @import ?







