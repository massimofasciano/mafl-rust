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

I tried to keep the language as functional as possible. Functions are the main compound type in MAFL. 
They are used to run code but also as objects and modules. The only exception to this is arrays. At first,
I tried using functions for arrays but decided to have a dedicated type instead.

By default, lexical binding is used. Closures are the main element in the language but the language also supports
special dynamic functions that expand at the call site. It has an @eval builtin that allows the execution
of a string as a program in the current context.

It would probably have been simpler and more efficient to use custom structured types (struct, class, object, etc...) than
to use the function paradigm for everything. I went for a model that was more fun to work with (objective 2).

In a lot of places in the language, storage cells can be multiply aliased, either by captured environments inside closures or by explicit reference objects.
That's obviously not good practice, especially coming from Rust, but it was fun to play with (again objective 2).

Currying is fully supported. That was very important to me. The language uses a declaration style without parentheses but function calls
use parentheses (makes it easier to call zero-argument functions). Function calls can be chained and the language re-arranges the arguments as they need to be consumed.


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
    forget i1 i2;
};

let state1 = builder(1,2);
# at this point state1 is an object containing
# - x: aliased to the x inside common
# - y,z: internal to state1 (values 1 and 2)
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

@println(state2);
state2.advance(2);
@println(state2);
state2.advance(2);
@println(state2);

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
State(11,4,8)
State(13,8,23)
State(15,16,44)
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
add custom builtin variables and functions to an interpreter instance. Communication is done
through variable bindings and a returned Value type.

In [examples/embed.rs](examples/embed.rs), we pass variable bindings (data) and a custom builtin function (Rust code) to the MAFL program.
We gather the result into a Value type that we extract into Rust variables to be further processed.

Here is the MAFL script that is used by the Rust program. It's an example of using a MAFL script as a "smart" config file for a Rust program.

```
 if not(@is_valid_subnet(subnet)) {
    @error("invalid subnet");
}

let name = {
    # if the NAME variable exists in the OS environment, use it for the name
    if (let env_name = @getenv("NAME")) != nil {
        exit env_name;
    }
    # otherwise if FILE exists in the OS environment, use it for the file name
    let file = "examples/hostnames.txt";
    if (let env_file = @getenv("FILE")) != nil {
        file = env_file;
    }
    from @std.iter use map filter;
    # get names from a file (remove extra whitespace and skip empty lines)
    let names = @lines(@readfile(file)) | map(@trim) | filter(\s{@len(s)>0});
    # grab a name at random
    names[@randint(0,@len(names)-1)]
};

subnet[3] = @randint(100,199);

let port = @randint(port_min, port_max);
forget port_min port_max;

# we return this object to Rust
module {
    use name subnet port;
}
```

each time the Rust program (embed) is run, it produces different results because our smart config file produces random values:

```
apple.local/192.168.1.190:8115
tomato.local/192.168.1.175:8183
apple.local/192.168.1.142:8192
tomato.local/192.168.1.172:8178
cherry.local/192.168.1.146:8128
tomato.local/192.168.1.134:8113
```

## The standard library vs builtin functions and variables

The language provides a series of builtin functions and variables. Their names start with @ (ex: @println). 
They are coded in Rust and receive special treatment by the interpreter: overloading and variable number of arguments are supported but currying is not.
More of these builtins can be added to an interpreter instance from Rust to customize behavior for embedding MAFL.

The language also has a standard library written in MAFL. It is visible as a module named @std.
Individual symbols can be imported into scope (ex: from @std.iter use range map).
A special prelude function can also be used to import a common set of symbols: @std.prelude()

In general, I chose to write the standard library functions in MAFL when it was possible and fun to do so. 
Sometimes, it was necessary to use the host language (Rust) for thigs like io, random, time.
The @std.builtin module contains MAFL bindings of Rust-based functions (ex: let len = fun x { @len(x) }).
This can be useful when currying is desired.
The @std.methods module contains bindings that are called on internal types by the interpreter when method calls are used (ex: 2.exp()).

By default, no bindings from @std are imported into the user variable space.

A this time, the MFAL standard library resides in [src/std.mafl](src/std.mafl) and is statically included in uncompressed form
inside the Rust binary at compile time. As it grows, this might not be ideal. Comments could be stripped and the text could be compressed.
It could also be pre-parsed to an AST and dumped in binary form.
Having it inside the binary image removes an external dependancy but makes it impossible to change it without recompiling.
It's a tradeoff I decided to make to make things easier (single standalone binary)

If you don't like this, you have 2 options:

1) A cargo compile-time flag named "std_internal" controls this behavior. It's on by default.
If disabled, src/std.mfal is read at runtime instead and can be modified without recompiling.
You still refer to @std as usual but it's loaded at runtime instead of compile time.

2) It's still possible to distribute a copy of this file with the interpreter and load it at runtime with @include even if the internal one is present.
Simply load it into another module and use it when you don't want the internal version. 
You can even mix and match when testing new versions of standard library functions.

```
let my_std = module {
    # we create our own module containing the standard library
    # loaded at runtime.
    @include("src/std.mafl");
};

# use some functions from my_std
from my_std.iter use map range; 
# and use another from the internal copy of @std
from @std.math use add;

map(add(1),range(0,4))
```

## TODO list

Here are a few things that I should work on:

- use Serde to serialize/deserialize from Rust structures to MFAL types (for easier embedding)
- right now, we have 2 types: Value and Expr. Value is used to pass values from Rust and to Rust (it only represents simple data). Expr represents the full range of data that the interpreter works with. It represents atomic values, code, closures, references, etc... It would probably be cleaner to also have a dedicated type for code. A "fun" contruct is code, an "if" construct is code but a closure is not (it contains runtime state + code).
- Expr uses Gc pointers and interior mutability GcCell almost everywhere. This made it easier to represent potentially-cyclic objects from MFAL. This is not required everywhere, especially in static code (no cycles or multiple references). Sometimes, Box would be enough and sometimes a normal reference with a lifetime.
- improve the display and debug traits for Expr

Eventually, it would be interesting to see what performance gains can be obtained by
- using bytecode and a stack instead of tree-walking
- transpiling to another language that is closer to MFAL (functional with gc)
- maybe even producing machine code

The design of the language probably makes these optimizations difficult (especially dynamic evaluation) and I don't think I will be writing a JIT.
