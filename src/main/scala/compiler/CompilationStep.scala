package compiler

enum CompilationStep {
  case Lexing, Parsing, ContextCreation, TypeChecking, CodeGeneration
}
