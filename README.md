Rust parser and interpreter for the MFEL language

refer to syntax.mfel in the examples folder for a quick description of the language syntax with examples

assuming the exec file is called mfel-0.6.0.exe, to run the interpreter:

mfel-0.6.0.exe PROGRAM_NAME.mfel

this will run the program and print the result

if you omit the name of the program, it will drop you into an interactive loop that evaluates code as you type it (one full line at a time).
this is quite simplisitic and only meant for testing quick commands.

ex:

mfel-0.6.0.exe

MFEL> 1+1
2
MFEL> let x = 20; let y = 2; while x > 0 { x -= 1; y *= 2; }; [x,y]
[0,2097152]
