package compiler

import compiler.AnalysisContext
import compiler.CompilationStep.ContextCreation
import compiler.Errors.{CompilationError, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.{FunDef, StructDef}
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type
import lang.{BuiltInFunctions, FunctionSignature, StructSignature}

import scala.collection.mutable

final case class AnalysisContext(functions: Map[String, FunctionSignature], structs: Map[String, StructSignature])

object AnalysisContext {

  final class Builder(errorReporter: ErrorReporter) {
    private val functions: mutable.Map[String, FunctionSignature] = mutable.Map.empty
    private val structs: mutable.Map[String, StructSignature] = mutable.Map.empty

    def addFunction(funDef: FunDef): Unit = {
      val name = funDef.funName
      if (BuiltInFunctions.builtInFunctions.contains(name)) {
        errorReporter.push(new CompilationError(ContextCreation, s"function name '$name' conflicts with built-in function", funDef.getPosition))
      } else if (functions.contains(name)) {
        errorReporter.push(new CompilationError(ContextCreation, s"redefinition of function '$name'", funDef.getPosition))
      } else {
        functions.put(name, funDef.signature)
      }
    }

    def addStruct(structDef: StructDef): Unit = {
      val name = structDef.structName
      if (structs.contains(name)) {
        errorReporter.push(new CompilationError(ContextCreation, s"redefinition of struct '$name'", structDef.getPosition))
      } else {
        val fieldsMap = mutable.Map[String, Type]()
        for param <- structDef.fields do {
          if (param.tpe == VoidType || param.tpe == NothingType) {

          } else if (fieldsMap.contains(param.paramName)) {
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
      new AnalysisContext(functions.toMap, structs.toMap)
    }

  }

}
