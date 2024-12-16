# Compiler documentation

## Compiler stages

1. Frontend (lexer + parser): runs separately on each source file and creates the AST
2. Context creator: creates an `AnalysisContext` that contains all the signatures of all the functions, constants, types 
   and tests of the program
3. Type checker: performs type checking and writes the types of each expression into the AST
4. Lowerer: transforms the AST to reduce the number of different constructs that the backend will have to handle
5. Tail checker: checks that tail calls are indeed in tail position
6. Backend: emits the bytecode
