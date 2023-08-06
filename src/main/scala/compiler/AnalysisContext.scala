package compiler

import compiler.AnalysisContext
import compiler.CompilationStep.ContextCreation
import compiler.Errors.{CompilationError, Err, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.{ConstDef, FunDef, StructDef, TestDef}
import identifiers.{FunOrVarId, TypeIdentifier}
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type
import lang.{BuiltInFunctions, FunctionSignature, StructSignature, Types}

import scala.annotation.tailrec
import scala.collection.mutable

final case class AnalysisContext(
                                  functions: Map[FunOrVarId, FunctionSignature],
                                  structs: Map[TypeIdentifier, StructSignature],
                                  tests: Set[FunOrVarId],
                                  constants: Map[FunOrVarId, Type]
                                ){

  /**
   * Returns `true` iff `tpe` is known (primitive type, known struct or array of a known type)
   */
  // do not use @tailrec, even though Intellij suggests it: fails the CI
  def knowsType(tpe: Type): Boolean = {
    tpe match {
      case _: Types.PrimitiveType => true
      case Types.StructType(typeName, _) => structs.contains(typeName)
      case Types.ArrayType(elemType, _) => knowsType(elemType)
      case Types.UndefinedType => true
    }
  }

}

object AnalysisContext {

  final class Builder(errorReporter: ErrorReporter) {
    private val functions: mutable.Map[FunOrVarId, FunctionSignature] = mutable.Map.empty
    private val structs: mutable.Map[TypeIdentifier, StructSignature] = mutable.Map.empty
    private val tests: mutable.Set[FunOrVarId] = mutable.Set.empty
    private val constants: mutable.Map[FunOrVarId, Type] = mutable.Map.empty

    def addFunction(funDef: FunDef): Unit = {
      val name = funDef.funName
      if (BuiltInFunctions.builtInFunctions.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"function name '$name' conflicts with built-in function", funDef.getPosition))
      } else if (functions.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of function '$name'", funDef.getPosition))
      } else {
        functions.put(name, funDef.signature)
      }
    }

    def addStruct(structDef: StructDef): Unit = {
      val name = structDef.structName
      if (structs.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of struct '$name'", structDef.getPosition))
      } else {
        val fieldsMap = new mutable.LinkedHashMap[FunOrVarId, StructSignature.FieldInfo]()
        for param <- structDef.fields do {
          param.paramNameOpt match {
            case None =>
              errorReporter.push(Err(ContextCreation, "struct fields must be named", param.getPosition))
            case Some(paramName) =>
              if (param.tpe == VoidType || param.tpe == NothingType) {
                errorReporter.push(Err(ContextCreation, s"struct field cannot have type '${param.tpe}'", param.getPosition))
              } else if (fieldsMap.contains(paramName)) {
                errorReporter.push(Err(ContextCreation, s"duplicated field: '$paramName'", param.getPosition))
              } else {
                fieldsMap.put(paramName, StructSignature.FieldInfo(param.tpe, param.isReassignable))
              }
          }
        }
        val sig = StructSignature(name, fieldsMap)
        structs.put(name, sig)
      }
    }
    
    def addTest(testDef: TestDef): Unit = {
      import testDef.testName
      if (tests.contains(testName)){
        errorReporter.push(Err(ContextCreation, s"redefinition of test $testName", testDef.getPosition))
      } else {
        tests.addOne(testName)
      }
    }
    
    def addConstant(constDef: ConstDef): Unit = {
      import constDef.constName
      if (constants.contains(constName)){
        errorReporter.push(Err(ContextCreation, s"redefinition of constant $constName", constDef.getPosition))
      } else {
        // type is already known since value is a Literal
        constants(constName) = constDef.value.getType
      }
    }

    def build(): AnalysisContext = {
      functions.addAll(BuiltInFunctions.builtInFunctions)
      new AnalysisContext(functions.toMap, structs.toMap, tests.toSet, constants.toMap)
    }

  }

}
