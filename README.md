# Rattlesnake 🐍

A simple procedural programming language, statically typed and compiled to JVM bytecode

## Example project

A sudoku solver: https://github.com/ValentinAebi/Rattlesnake-sudoku-solver

## Example program
```
fn joinWords(words: arr String, endSymbol: Char) -> String {
    var joined = "";
    for var i = 0; i < #words; i += 1 {
        joined += words[i];
        if i < #words-1 {
            joined += " ";
        }
    };
    return joined + charToString(endSymbol)
}

fn main(arr String){
    val msgWords = ["Hello", "world"];
    val msg = joinWords(msgWords, '!');
    print(msg)   // displays "Hello world!"
}

```

[Compiler documentation here](CompilerDoc.md)

## Command-line program

Run help to see the available commands and options

## Language description [outdated]

The language does not support modules. All functions and data structures are top-level and identified by their textual name across the whole program. Functions and types can be referred to from inside the file that defines them or from another file in the exact same way.

### Code blocks

Statements are separated with `;`. `;` may be omitted after the last statement.

### Types and data structures

- Primitive types:
    - `Int`, `Double`, `Bool`, `Char`
    - `Void`: return type of a function that does not return any value
    - `Nothing`: return type of a function that terminates the program and thus never returns

    Subtyping: there is no subtyping relation, except that `Nothing` is a subtype of all other types

- `String`

- Arrays: `arr <element type>`, e.g. `arr Int`

    Creation: <p>
    - `arr <type>[<size>]`, e.g. `arr Int[0]` <p>
    - `[<elems>*], e.g. [-7, 31, 14, 11]`
    
    Access an element: `<array>[<index>]`, e.g. `xs[7]`
    
- Structures, e.g. `struct Foo { bar: Int }`

    Creation: `new Foo { 0 }`
    
    Access a field: `foo.bar`
    
### Functions

```
fn <function name>(<args>*) -> <return type> {
   ...
}
```
Return type can be omitted if it is `Void`
E.g.:
```
fn bar(i: Int, b: Bool) -> String {
   ...
   return "Hello"
}
```
A non-void function must contain a `return <value>` for each control-flow path in the function. If the function has return type `Void`, then `return` is not required but may be used (without a value) to exit the function early.

### Locals

```
val <name>: <type> = ...
```
Type may be omitted. `var`s are defined similarly.
E.g.:
```
val x: Int = 0;
val str = "Rattlesnake";
```
`var`s (but not `val`s) can be reassigned: `x = <new value>`

### Constants

Constants can only be of primitive types. Their name must be lowercase.
```
const <name>: <type> = <value>
```
Type may be omitted. E.g.:
```
const anwser = 42
```


### Control structures

#### If-else
```
if <cond> {
   ...
}
if <cond> {
   ...
} else if <cond> {
   ...
} else {
   ...
}
```

#### While loop
```
while <cond> {
   ...
}
```

#### For loop
```
for <stat>;<cond>;<stat> {
   ...
}
```
E.g.:
```
for var i = 0; i < #array; i += 1 {
   ...
}
```

### Operators

#### Unary operators
- `-`: opposite of an `Int` or a `Double`
- `!`: logical negation of a `Bool`

#### Binary operators
- Mathematical: `+`, `-`, `*`, `/`, `%` (can be combined with `=`: `+=`, `/=`, etc.)
- Comparisons (between `Int`s or `Double`s): `<`, `>`, `>=`, `<=`
- Equality: `==`, `!=` (strings are compared by structure, `struct`s are compared by reference)
- Logical: `&&`, `||` (implement lazy evaluation of second operand)
- String concatenation: `+`

#### Ternary operator
```
when <cond> then <expr1> else <expr2>
```

#### Cast/type conversion
The following conversions can be performed:
- `Int` <-> `Char`
- `Int` <-> `Double`

Syntax: `<expr> as <type>`, e.g. `10 as Double`

#### Panic
Terminates the program with an exception
`panic <message>`, e.g. `panic "forbidden argument " + arg`

## Built-in functions

The compiler replaces calls to these functions with special instructions.

- `print(s: String)`: display `s` on the console
- `?ToString(...)` with `?` one of `int`, `double`, `char`, `bool`, and the corresponding parameter type: conversion to a string
- `toCharArray(s: String)`: converts a string into an array of all its characters

## Tests

Tests are defined similarly to functions, but `fn` is replaced by `test` and there is no parameters list. Failures are reported using `panic`:
```
test exampleTest {
    val i = 1 + 1;
    if i != 2 {
        panic "We have a problem!"
    }
}
```

## References

Lexer and parser are inspired from https://github.com/epfl-lara/silex and https://github.com/epfl-lara/scallion, respectively.

Backend uses the ASM bytecode manipulation library: https://asm.ow2.io/

