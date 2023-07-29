# Compiler documentation

## Compiler stages

1. Frontend (lexer + parser): runs separately on each source file and creates the AST
2. Context creator: creates an `AnalysisContext` that contains all the functions and structs of the program
3. Type checker: performs type checking and writes the types of each expression into the AST (side effect)
4. Lowerer: transforms the AST to reduce the number of different constructs that the backend will have to handle
5. Backend: produces the JVM bytecode, either as `.class` files or as textual instructions, depending on its `Mode`

![](CompilerStagesDiagram.png)

