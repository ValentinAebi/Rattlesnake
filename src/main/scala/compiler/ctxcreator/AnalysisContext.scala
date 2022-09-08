package compiler.ctxcreator

import compiler.CompilationStep.ContextCreation
import compiler.Errors.{CompilationError, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.{FunDef, StructDef}
import lang.BuiltInFunctions
import lang.Types.PrimitiveType.VoidType
import lang.Types.Type

import scala.collection.mutable
import lang.FunctionSignature
import lang.StructSignature

final case class AnalysisContext(
                                  functions: Map[String, FunctionSignature],
                                  structs: Map[String, StructSignature],
                                  locals: mutable.Map[String, (Type, Boolean)]
                                ){
  def copyWithoutLocals: AnalysisContext = {
    copy(locals = mutable.Map.empty)
  }

  def copied: AnalysisContext = {
    copy(locals = mutable.Map.from(locals))
  }

  def addLocal(name: String, tpe: Type, mutable: Boolean, duplicateVarCallback: () => Unit): Unit = {
    if (locals.contains(name)){
      duplicateVarCallback()
    } else {
      locals.put(name, (tpe, mutable))
    }
  }
}

object AnalysisContext {

  final class Builder(errorReporter: ErrorReporter) {
    private val functions: mutable.Map[String, FunctionSignature] = mutable.Map.empty
    private val structs: mutable.Map[String, StructSignature] = mutable.Map.empty

    def addFunction(funDef: FunDef): Unit = {
      val name = funDef.funName
      if (BuiltInFunctions.builtInFunctions.contains(name)){
        errorReporter.push(new CompilationError(ContextCreation, s"function name '$name' conflicts with built-in function", funDef.getPosition))
      } else if (functions.contains(name)) {
        errorReporter.push(new CompilationError(ContextCreation, s"redefinition of function '$name'", funDef.getPosition))
      } else {
        val sig = FunctionSignature(name, funDef.params.map(_.tpe), funDef.optRetType.getOrElse(VoidType))
        functions.put(name, sig)
      }
    }

    def addStruct(structDef: StructDef): Unit = {
      val name = structDef.structName
      if (structs.contains(name)) {
        errorReporter.push(new CompilationError(ContextCreation, s"redefinition of struct '$name'", structDef.getPosition))
      } else {
        val fieldsMap = mutable.Map[String, Type]()
        for param <- structDef.fields do {
          if (fieldsMap.contains(param.paramName)){
            errorReporter.push(new CompilationError(ContextCreation, s"duplicated field: '${param.paramName}'", param.getPosition))
          } else {
            fieldsMap.put(param.paramName, param.tpe)
          }
        }
        val sig = StructSignature(name, fieldsMap.toMap)
        structs.put(name, sig)
      }
    }

    def build(): AnalysisContext = {
      functions.addAll(BuiltInFunctions.builtInFunctions)
      new AnalysisContext(functions.toMap, structs.toMap, mutable.Map.empty)
    }

  }

}
