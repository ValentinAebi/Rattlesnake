package compiler

import compiler.AnalysisContext
import compiler.CompilationStep.ContextCreation
import compiler.Errors.{CompilationError, Err, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.{ConstDef, FunDef, StructDef, TestDef}
import lang.SubtypeRelation.subtypeOf
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
                                ) {

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
    private val structs: mutable.Map[TypeIdentifier, (StructSignature, Option[Position])] = mutable.Map.empty
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
        val sig = StructSignature(name, fieldsMap, structDef.directSupertypes, structDef.isInterface)
        structs.put(name, (sig, structDef.getPosition))
      }
    }

    def addTest(testDef: TestDef): Unit = {
      import testDef.testName
      if (tests.contains(testName)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of test $testName", testDef.getPosition))
      } else {
        tests.addOne(testName)
      }
    }

    def addConstant(constDef: ConstDef): Unit = {
      import constDef.constName
      if (constants.contains(constName)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of constant $constName", constDef.getPosition))
      } else {
        // type is already known since value is a Literal
        constants(constName) = constDef.value.getType
      }
    }

    def build(): AnalysisContext = {
      val builtStructsMap = structs.map((tid, sigAndPos) => (tid, sigAndPos._1)).toMap
      checkSubtyping(builtStructsMap)
      functions.addAll(BuiltInFunctions.builtInFunctions)
      new AnalysisContext(
        functions.toMap,
        builtStructsMap,
        tests.toSet,
        constants.toMap
      )
    }

    private def checkSubtyping(builtStructMap: Map[TypeIdentifier, StructSignature]): Unit = {
      for {
        (structId, (structSig, posOpt)) <- structs
        directSupertypeId <- structSig.directSupertypes
      } {
        structs.get(directSupertypeId) match {
          case Some((directSupertypeSig, _)) => {
            if (directSupertypeSig.isInterface) {
              for ((fldName, superFldInfo) <- directSupertypeSig.fields) {
                structSig.fields.get(fldName) match
                  case Some(subFieldInfo) =>
                    if (logicalImplies(superFldInfo.isReassignable, subFieldInfo.isReassignable)) {
                      checkFieldVariance(structId, directSupertypeId, fldName, subFieldInfo, superFldInfo, builtStructMap, posOpt)
                    } else {
                      errorReporter.push(Err(ContextCreation,
                        s"subtyping error: $fldName needs to be reassignable in subtypes of $superFldInfo", posOpt))
                    }
                  case None =>
                    errorReporter.push(Err(ContextCreation,
                      s"$structId cannot extend $directSupertypeId: missing field $fldName", posOpt))
              }
            } else {
              errorReporter.push(Err(ContextCreation, s"struct $directSupertypeId is not an interface", posOpt))
            }
          }
          case None =>
            errorReporter.push(Err(ContextCreation, s"interface $directSupertypeId is unknown", posOpt))
        }
      }
    }

    private def checkFieldVariance(
                                    structId: TypeIdentifier, directSupertypeId: TypeIdentifier,
                                    fldName: FunOrVarId,
                                    subFieldInfo: StructSignature.FieldInfo, superFldInfo: StructSignature.FieldInfo,
                                    builtStructMap: Map[TypeIdentifier, StructSignature], posOpt: Option[Position]
                                  ): Unit = {
      if (superFldInfo.isReassignable && subFieldInfo.tpe != superFldInfo.tpe) {
        errorReporter.push(Err(ContextCreation,
          s"subtyping error: type of $fldName is not the same in $structId and $directSupertypeId", posOpt))
      } else if (!superFldInfo.isReassignable && !subFieldInfo.tpe.subtypeOf(superFldInfo.tpe)(using builtStructMap)) {
        errorReporter.push(Err(ContextCreation,
          s"subtyping error: type of $fldName in $structId should be a subtype of its type in $directSupertypeId", posOpt))
      }
    }

    private def logicalImplies(p: Boolean, q: Boolean): Boolean = !p || q

  }

}
