package compiler.pipeline

enum CompilationStep {
  case Lexing, Parsing, ContextCreation, TypeChecking, CaptureChecking, CodeGeneration, SourceFileWriting
}
