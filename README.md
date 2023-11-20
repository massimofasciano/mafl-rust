# Rust parser and interpreter for the MAFL language

When I created the MAFL language, I had 2 main objectives:
1) Creating a useful embeddable functional scripting tool (usable from Rust)
2) Having fun and learning about parsing and interpreting a language and also using Rust smart pointers and garbage collection.

Objective 2 was more important to me. This means that the design of the MAFL programming language is far from perfect.
Performance is not very good: i'm using a tree-walking interpreter, the language creates cyclic structures and I'm using the Gc crate
to perform garbage collection (Rc does not like cycles).

I tried keep the language as functional as possible. Functions are the main compound type in MAFL. 
They are used to run code but also as objects and modules. The only exception to this is arrays. At first,
I tried using functions for arrays but decided to have a dedicated type instead.

By default, lexical binding is used. Closures are the main element in the language. But the language also supports
special dynamic functions that expand at the call site. It has an @eval builtin that allows the execution
of a string as a program in the current context.

Refer to [mfal/language.mfal](mfal/language.mfal) for a walkthrough of the MFAL language as a long MFAL program.

The interpreter takes a single argument: the name of the program to run. It runs it and shows the result as a value.
When unit tests are performed, it produces a summary of pass/fail counts.

```bash
$ mafl mafl/language.mafl
*** UNIT TESTING ***
# test passed 2 + 3: result 5
# test passed 2 + 3 < 2 * 3
# test passed @test_pass_count: result 2
# test passed @test_fail_count: result 0
*** LET ***
# warning: shadowed x in local context.
*** ASSIGN ***
# test passed x: result 10
...
*** Program result as a value:
true

*** UNIT TEST SUMMARY:
182 passed. 0 failed.
```

If no arguments are provided, The interpreter enters into a read-eval-print loop. 
Expressions have to fit on a single line and no editing of previous expressions is possible. 

```
$ mafl
MAFL> 1+1
2
MAFL> let x = 10; let f = fun a { a * 10 };
nil
MAFL> f(x)+5
105
MAFL>
```

The examples folder shows how to embed the MFAL interpreter into a Rust program. It's possible to 
add custom builtin variables and functions to the bound interpreter. Communication is done
through variable bindings and a returned Value type.

