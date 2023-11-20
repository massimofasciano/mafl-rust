# Rust parser and interpreter for the MAFL language

## Objectives

When I created the MAFL language, I had 2 main objectives:
1) Creating a useful embeddable functional scripting tool. This can be used to write "smart" config files
for a compiled Rust program or short user-editable scripts that call back to the Rust code via custom functions.
2) Having fun and learning about parsing and interpreting a language and also using Rust smart pointers and garbage collection. 
Keeping the language as functional (but not pure) as possible because I like functional languages. 
Mutability and aliasing are ok because they create the opportunity of working with more complex data structures in Rust.

Objective 2 was more important to me. This means that the design of the MAFL programming language is far from perfect.
Performance is not very good: i'm using a tree-walking interpreter, the language creates cyclic structures and I'm using the Gc crate
to perform garbage collection (Rc does not like cycles).

I tried keep the language as functional as possible. Functions are the main compound type in MAFL. 
They are used to run code but also as objects and modules. The only exception to this is arrays. At first,
I tried using functions for arrays but decided to have a dedicated type instead.

By default, lexical binding is used. Closures are the main element in the language. But the language also supports
special dynamic functions that expand at the call site. It has an @eval builtin that allows the execution
of a string as a program in the current context.

It would probably have been simpler and more efficient to use custom structured types (struct, class, object, etc...) than
to use the function paradigm for everything. I went for a model that was more fun to work with (objective 2).

In a lot of places in the language, storage cells can be multiply aliased, either by captured environments inside closures or by explicit reference objects.
That's obviously not good practice, especially coming from Rust, but it was fun to play with (again objective 2).

Currying is fully supported. That was very important to me. The language uses a declaration style without parentheses but function calls
use parentheses (makes it easier to call zero-argument functions). Function calls can be chained and the language re-arranges the arguments they need to be consumed.

## Examples

Refer to [mafl/language.mafl](mafl/language.mafl) for a walkthrough of the MFAL language as a long MFAL program.

A first functional example. The fun keyword is used to create closures (lambdas). It can be replaced with a more compact form with backslash.

```
let arr = [1,2,3,4];
let f = fun x xs { 
    @println(
        map(add(2),xs),
        map(flip(div,2),xs)
    ); # 2 results printed
    fold(\x,y{x/2+y*5},x,xs) # no semicolon at end (implicit return from the function)
};
f(10,arr) # no semicolon at end (implicit return from the program)
```
produces the following output:

```
[3, 4, 5, 6] [0.5, 1, 1.5, 2]

*** Program result as a value:
31.25
```

Functions as objects. 
cons is a fun that returns its captured state as an object (object factory). 
module is like a zero-arg cons that is called immediately (instance creator).

```
let common = module {
    let x = 10;
};
let builder = cons i1 i2 {
    from common use x;
    let y = i1;
    let z = i2;
    let advance = fun n {
        z = z + y + x;
        x += n;
        y *= n;
    };
    let reset = fun {
        y = i1;
        z = i2;
    };
    let to_string = fun {
        "State(" + x + "," + y + "," + z + ")"
    };
    forget i1;
    forget i2;
};
let state1 = builder(1,2);
# at this point state1 is an object containing
# - x: aliased to the x inside common
# - y,z: internal to state1
# - 3 methods: advance, reset and to_string (called by @println)
# - no i1 or i2 because we forgot them
# but the reset method has captured i1 and i2 in a closure and can use them
# even if the parent forgets them.

let state2 = builder(4,8);
# another instance (with x shared through common)

@println(state1);
@println(state2);

state1.x += 1;
test common.x expect 11;
test state1.x expect 11;
test state2.x expect 11;

state2.advance(2);
state2.advance(2);
test common.x expect 15;
test state1.x expect 15;
test state2.x expect 15;

@println(state1);
@println(state2);

state2.reset();
@println(state1);
@println(state2);
```
produces the following output:

```
State(10,1,2)
State(10,4,8)
# test passed common.x: result 11
# test passed state1.x: result 11
# test passed state2.x: result 11
# test passed common.x: result 15
# test passed state1.x: result 15
# test passed state2.x: result 15
State(15,1,2)
State(15,16,44)
State(15,1,2)
State(15,4,8)
```

## The interpreter


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

## Embedding

The examples folder shows how to embed the MFAL interpreter into a Rust program. It's possible to 
add custom builtin variables and functions to the bound interpreter. Communication is done
through variable bindings and a returned Value type.

