Rust parser and interpreter for the MFEL language

FEATURES:
- added internal stdlib (coded in mfel)
- mutable arrays with @array, @len, @append and literals
- Error type (detectable to catch errors)
- object factory

TODO:

- eliminate defun and all def... ? add letrec instead ?

- array assign and let rec parallel
[a,b,c] = [1,2,3]

let rec [a,b,c] = [1,2,3];   
IS
let [a,b,c] = [nil,nil,nil];
[a,b,c] = [1,2,3];
OR
let a; let b; let c;
a = [1,2,3][0];
b = [1,2,3][1];
c = [1,2,3][2];
(check lengths before)

- field (object) based builtins like .copy() .string() .map() ?

- curry @builtin and op (LUT for builtins with fixed arg list, op syntax maybe ?)
- builtins and ops should behave like lambdas

- try a tree structure for contexts: NO! a stack of stacks (list of lists) is a better analogy.
- walker for expression and context with cycle detection

- clean up the expression type (atoms, code, input, etc...)

- weak references ? gc ?

- lookup table for builtins ?
- put all builtins inside the interpreter struct
