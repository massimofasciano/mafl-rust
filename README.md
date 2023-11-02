Rust parser and interpreter for the MFEL language

FEATURES:
- added staticfn (no capture) vs closure (captures context at definition) vs function (captures context at application)
- added external stdlib example (coded in mfel)
- mutable arrays with @array, @len, @append and literals

TODO:
- context as hashmaps (get/set)
- add Character type and String to Array of Char
- context inheritance syntax
- varargs
- general while/for loop
- foreach and iterator (returns unit or value)
- make builtin @id return an expression (a closure if functional) ?
- builtin STDLIB (returns struct) ?

- ref and deref (by name ?)
- implement return continue break

- eval gets ref in and outputs owned: would be easier with owned/owned but maybe it's impossible ?
