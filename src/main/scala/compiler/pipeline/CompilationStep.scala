package compiler.pipeline

enum CompilationStep {
  case Lexing, Parsing, ContextCreation, TypeChecking, PathsChecking, CodeGeneration, SourceFileWriting
}
