# Rattlesnake 🐍

Statically typed imperative toy programming language, compiled to the JVM

```
fn joinWords(words: arr String, endSymbol: String) -> String {
    var joined = "";
    for var i = 0; i < #words; i += 1 {
        joined += words[i];
        if i < #words-1 {
            joined += " ";
        }
    };
    return joined + endSymbol
}

fn main(args: arr String){
    val msgWords = ["Hello", "world"];
    val msg = joinWords(msgWords, "!");
    print(msg)   // displays "Hello world!"
}
```

## References

Lexer and parser are inspired from https://github.com/epfl-lara/silex and https://github.com/epfl-lara/scallion, respectively.

Backend uses the ASM bytecode manipulation library: https://asm.ow2.io/

