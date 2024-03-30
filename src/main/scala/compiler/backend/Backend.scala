package compiler.backend

import compiler.CompilationStep.CodeGeneration
import compiler.Errors.*
import compiler.backend.BuiltinFunctionsImpl.*
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.backend.TypesConverter.{convertToAsmType, convertToAsmTypeCode, internalNameOf, opcodeFor}
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FileExtensions, GenFilesNames}
import identifiers.{BackendGeneratedVarId, FunOrVarId, TypeIdentifier}
import lang.Operator.*
import lang.SubtypeRelation.subtypeOf
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, PrimitiveType, StructType, UnionType}
import lang.*
import org.objectweb.asm
import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.{Nil, StringOps}
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

/**
 * Generates the output files: 1 core file, containing the program, and 1 file per struct
 *
 * @param mode          cf nested class [[Backend.Mode]]
 * @param outputDirBase output directory path (will be extended with `/out`)
 * @param outputName    name of the output file
 * @tparam V depends on the mode
 */
final class Backend[V <: ClassVisitor](
                                        mode: Backend.Mode[V],
                                        errorReporter: ErrorReporter,
                                        outputDirBase: Path,
                                        javaVersionCode: Int,
                                        outputName: String,
                                        functionsToInject: List[FunDef],
                                        generateTests: Boolean
                                      ) extends CompilerStep[(List[Source], AnalysisContext), List[Path]] {

  import Backend.*
  
  override def apply(input: (List[Source], AnalysisContext)): List[Path] = {
    val (sources, analysisContext) = input
    if (sources.isEmpty) {
      errorReporter.pushFatal(Fatal(CodeGeneration, "nothing to write: no sources", None))
    }
    else {

      given Map[TypeIdentifier, StructSignature] = analysisContext.structs

      val functionsBuilder = List.newBuilder[FunDef]
      val structsBuilder = List.newBuilder[StructDef]
      val testsBuilder = List.newBuilder[TestDef]
      val constsBuilder = List.newBuilder[ConstDef]
      for src <- sources; df <- src.defs do {
        df match
          case funDef: FunDef => functionsBuilder.addOne(funDef)
          case structDef: StructDef => structsBuilder.addOne(structDef)
          case testDef: TestDef => testsBuilder.addOne(testDef)
          case constDef: ConstDef => constsBuilder.addOne(constDef)
      }
      val functions = functionsBuilder.result()
      // sort the structs so that no class is loaded before its super-interfaces
      val structs = sortStructs(structsBuilder.result())
      val tests = testsBuilder.result()
      val consts = constsBuilder.result()

      // create output directory if it does not already exist
      val outputDir = outputDirBase.resolve("out")
      Files.createDirectories(outputDir)

      val structFilesPaths = generateStructs(structs, outputDir, analysisContext)

      val coreFilePath = outputDir.resolve(mode.withExtension(outputName))
      generateCoreFile(analysisContext, functions, coreFilePath)

      val constantsFilePath = outputDir.resolve(mode.withExtension(GenFilesNames.constantsFileName))
      generateConstantsFile(consts, constantsFilePath)

      val testFilePath = outputDir.resolve(mode.withExtension(GenFilesNames.testFileName))
      if (generateTests) {
        generateTestsFile(analysisContext, tests, testFilePath)
      }

      errorReporter.displayAndTerminateIfErrors()
      (if generateTests then List(testFilePath) else Nil) ++ (coreFilePath :: constantsFilePath :: structFilesPaths)
    }

  }

  /**
   * Topological sort on the subtyping relation: each supertype appears before its subtypes
   */
  private def sortStructs(structs: List[StructDef]): List[StructDef] = {
    val remStructs = mutable.Queue.from(structs)
    val sortedList = new mutable.LinkedHashMap[TypeIdentifier, StructDef]
    while (remStructs.nonEmpty){
      // get is safe here because the subtyping relation admits no cycles (checked by ContextCreation phase)
      val curr = remStructs.removeFirst(_.directSupertypes.forall(sortedList.contains)).get
      sortedList.addOne(curr.structName -> curr)
    }
    sortedList.toList.map(_._2)
  }

  private def generateTestsFile(analysisContext: AnalysisContext, tests: List[TestDef], testFilePath: Path)
                               (using Map[TypeIdentifier, StructSignature]): Unit = {
    val tv: V = mode.createVisitor(testFilePath)
    tv.visit(javaVersionCode, ACC_PUBLIC, GenFilesNames.testFileName, null, objectTypeStr, null)
    for test <- tests do {
      val mv = tv.visitMethod(
        ACC_PUBLIC | ACC_STATIC,
        GenFilesNames.testMethodPrefix ++ test.testName.stringId,
        "()Z",
        null,
        null
      )
      generateTest(test, mv, analysisContext, outputName)
    }
    tv.visitEnd()
    mode.terminate(tv, testFilePath, errorReporter)
  }

  private def generateConstantsFile(consts: List[ConstDef], constantsFilePath: Path)
                                   (using Map[TypeIdentifier, StructSignature]): Unit = {
    val ccv: V = mode.createVisitor(constantsFilePath)
    ccv.visit(javaVersionCode, ACC_PUBLIC, GenFilesNames.constantsFileName, null, objectTypeStr, null)
    for const <- consts do {
      ccv.visitField(
        ACC_PUBLIC | ACC_STATIC,
        const.constName.stringId,
        descriptorForType(const.value.getType),
        null,
        const.value.value
      )
    }
    ccv.visitEnd()
    mode.terminate(ccv, constantsFilePath, errorReporter)
  }

  private def generateCoreFile(analysisContext: AnalysisContext, functions: List[FunDef], coreFilePath: Path)
                              (using Map[TypeIdentifier, StructSignature]): Unit = {
    val cv: V = mode.createVisitor(coreFilePath)
    cv.visit(javaVersionCode, ACC_PUBLIC, outputName, null, objectTypeStr, null)
    for function <- (functions ++ functionsToInject) do {
      val mv = cv.visitMethod(ACC_PUBLIC | ACC_STATIC, function.funName.stringId, descriptorForFunc(function.signature), null, null)
      generateFunction(function, mv, analysisContext, outputName)
    }
    cv.visitEnd()
    mode.terminate(cv, coreFilePath, errorReporter)
  }

  private def generateStructs(structs: List[StructDef], outputDir: Path, ctx: AnalysisContext)
                             (using Map[TypeIdentifier, StructSignature]): List[Path] = {
    val structFilesPaths = {
      for struct <- structs yield {
        val structFilePath = outputDir.resolve(mode.withExtension(struct.structName.stringId))
        val interfaces = struct.directSupertypes.map(_.stringId).toArray
        generateStruct(struct, interfaces, structFilePath, ctx)
        structFilePath
      }
    }
    structFilesPaths
  }

  private def generateStruct(structDef: StructDef, superInterfaces: Array[String],
                             structFilePath: Path, ctx: AnalysisContext)
                            (using structs: Map[TypeIdentifier, StructSignature]): Unit = {
    val structName = structDef.structName
    val sig = structs.apply(structName)
    val isInterface = sig.isInterface
    val cv = mode.createVisitor(structFilePath)
    var classMods = ACC_PUBLIC
    if (isInterface){
      classMods |= ACC_INTERFACE
      classMods |= ACC_ABSTRACT
    } else {
      classMods |= ACC_FINAL
    }
    cv.visit(javaVersionCode, classMods, structDef.structName.stringId, null, objectTypeStr, superInterfaces)
    if (!isInterface){
      addFields(structDef, cv)
      addConstructor(cv)
    }
    val structSig = ctx.structs(structName)
    val fieldsWithAccessors = if isInterface then sig.fields.keySet else getInterfaceFieldsForStruct(structName, ctx.structs)
    for (fld <- fieldsWithAccessors) {
      val fldType = structSig.fields.apply(fld).tpe
      val fieldDescr = descriptorForType(fldType)
      generateGetter(structName, fld, fldType, fieldDescr, cv, genImplementation = !isInterface)
      if (structSig.fields.apply(fld).isReassignable) {
        generateSetter(structName, fld, fldType, fieldDescr, cv, genImplementation = !isInterface)
      }
    }
    cv.visitEnd()
    mode.terminate(cv, structFilePath, errorReporter)
  }

  private def generateGetter(structName: TypeIdentifier, fld: FunOrVarId, fldType: Types.Type,
                             fieldDescr: String, cv: ClassVisitor, genImplementation: Boolean)
                            (using Map[TypeIdentifier, StructSignature]): Unit = {
    val getterDescriptor = descriptorForFunc(FunctionSignature(fld, List.empty, fldType))
    var modifiers = ACC_PUBLIC
    if (!genImplementation){
      modifiers |= ACC_ABSTRACT
    }
    val getterVisitor = cv.visitMethod(modifiers, fld.stringId,
      getterDescriptor, null, null)
    if (genImplementation) {
      getterVisitor.visitCode()
      getterVisitor.visitVarInsn(Opcodes.ALOAD, 0)
      getterVisitor.visitFieldInsn(Opcodes.GETFIELD, structName.stringId, fld.stringId, fieldDescr)
      getterVisitor.visitInsn(opcodeFor(fldType, Opcodes.IRETURN, Opcodes.ARETURN))
      getterVisitor.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    }
    getterVisitor.visitEnd()
  }

  private def generateSetter(structName: TypeIdentifier, fld: FunOrVarId, fldType: Types.Type,
                             fieldDescr: String, cv: ClassVisitor, genImplementation: Boolean)
                            (using Map[TypeIdentifier, StructSignature]): Unit = {
    val setterDescriptor = descriptorForFunc(FunctionSignature(fld, List(fldType), PrimitiveType.VoidType))
    var modifiers = ACC_PUBLIC
    if (!genImplementation) {
      modifiers |= ACC_ABSTRACT
    }
    val setterVisitor = cv.visitMethod(modifiers, fld.stringId, setterDescriptor, null, null)
    if (genImplementation) {
      setterVisitor.visitCode()
      setterVisitor.visitVarInsn(Opcodes.ALOAD, 0)
      setterVisitor.visitVarInsn(opcodeFor(fldType, Opcodes.ILOAD, Opcodes.ALOAD), 1)
      setterVisitor.visitFieldInsn(Opcodes.PUTFIELD, structName.stringId, fld.stringId, fieldDescr)
      setterVisitor.visitInsn(Opcodes.RETURN)
      setterVisitor.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    }
    setterVisitor.visitEnd()
  }

  private def addConstructor(cv: ClassVisitor): Unit = {
    val constructorVisitor = cv.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
    constructorVisitor.visitCode()
    constructorVisitor.visitVarInsn(Opcodes.ALOAD, 0)
    constructorVisitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    constructorVisitor.visitInsn(Opcodes.RETURN)
    constructorVisitor.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    constructorVisitor.visitEnd()
  }

  private def addFields(structDef: StructDef, cv: ClassVisitor)
                       (using Map[TypeIdentifier, StructSignature]): Unit = {
    for field <- structDef.fields do {
      val fieldVisitor = cv.visitField(Opcodes.ACC_PUBLIC, field.paramNameOpt.get.stringId, descriptorForType(field.tpe), null, null)
      fieldVisitor.visitEnd()
    }
  }

  private def generateFunction(funDef: FunDef, mv: MethodVisitor, analysisContext: AnalysisContext, outputName: String)
                              (using Map[TypeIdentifier, StructSignature]): Unit = {
    val ctx = CodeGenerationContext.from(analysisContext)
    val unnamedParamIdx = new AtomicInteger(0)
    for param <- funDef.params do {
      param.paramNameOpt match {
        case None =>
          ctx.addLocal(BackendGeneratedVarId(unnamedParamIdx.incrementAndGet()), param.tpe)
        case Some(paramName) =>
          ctx.addLocal(paramName, param.tpe)
      }
    }
    mv.visitCode()
    generateCode(funDef.body, ctx)(mv, outputName)
    if (ctx.analysisContext.functions.apply(funDef.funName).retType == VoidType) {
      mv.visitInsn(Opcodes.RETURN)
    }
    mv.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    mv.visitEnd()
  }

  private def generateTest(testDef: TestDef, mv: MethodVisitor, analysisContext: AnalysisContext, outputName: String)
                          (using Map[TypeIdentifier, StructSignature]): Unit = {
    val ctx = CodeGenerationContext.from(analysisContext)
    val start = new Label()
    val end = new Label()
    val handler = new Label()
    mv.visitCode()
    mv.visitTryCatchBlock(start, end, handler, null)
    mv.visitLabel(start)
    generateCode(testDef.body, ctx)(mv, outputName)
    generateCode(printCallFor(StringLit(s"    Test ${testDef.testName} PASSED\n")), ctx)(mv, outputName)
    mv.visitLdcInsn(true)
    mv.visitInsn(Opcodes.IRETURN)
    mv.visitLabel(end)
    mv.visitLabel(handler)
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Throwable", "getMessage", "()Ljava/lang/String;", false)
    mv.visitVarInsn(Opcodes.ASTORE, ctx.currLocalIdx)
    val msgVarName = BackendGeneratedVarId("msg")
    ctx.addLocal(msgVarName, StringType)
    generateCode(
      printCallFor(StringLit("/!\\ Test "), StringLit(testDef.testName.stringId), StringLit(" FAILED: "), VariableRef(msgVarName), StringLit("\n")),
      ctx
    )(mv, outputName)
    mv.visitLdcInsn(false)
    mv.visitInsn(Opcodes.IRETURN)
    mv.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    mv.visitEnd()
  }

  private def printCallFor(msgParts: Expr*): Call = {
    Call(VariableRef(BuiltInFunctions.print), List(msgParts.reduceLeft(BinaryOp(_, Plus, _).setType(StringType))))
  }

  private def getInterfaceFieldsForStruct(structId: TypeIdentifier, structs: Map[TypeIdentifier, StructSignature]): Set[FunOrVarId] = {
    // BFS

    val fields = mutable.Set.empty[FunOrVarId]
    val workList = mutable.Queue.empty[TypeIdentifier]
    val alreadyAdded = mutable.Set.empty[TypeIdentifier]

    workList.addAll(structs.apply(structId).directSupertypes)
    while (workList.nonEmpty) {
      val curr = structs(workList.dequeue())
      fields.addAll(curr.fields.keys)
      val toAdd = curr.directSupertypes.filterNot(alreadyAdded.contains)
      workList.addAll(toAdd)
      alreadyAdded.addAll(toAdd)
    }
    fields.toSet
  }

  private def generateCode(ast: Ast, ctx: CodeGenerationContext)
                          (using Map[TypeIdentifier, StructSignature])
                          (implicit mv: MethodVisitor, outputName: String): Unit = { // TODO move to using
    val analysisContext = ctx.analysisContext
    ast match {

      case Block(stats) => generateSequence(ctx, stats, optFinalExpr = None)
      case Sequence(stats, expr) => generateSequence(ctx, stats, Some(expr))

      case LocalDef(varName, tpeOpt, rhs, _) =>
        generateCode(rhs, ctx)
        val opcode = opcodeFor(rhs.getType, Opcodes.ISTORE, Opcodes.ASTORE)
        mv.visitVarInsn(opcode, ctx.currLocalIdx)
        ctx.addLocal(varName, tpeOpt.get)

      case IntLit(value) => mv.visitLdcInsn(value)
      case DoubleLit(value) => mv.visitLdcInsn(value)
      case CharLit(value) => mv.visitLdcInsn(value)
      case BoolLit(value) => mv.visitLdcInsn(if value then 1 else 0)
      case StringLit(value) => mv.visitLdcInsn(value)

      case varRef@VariableRef(name) => {
        ctx.getLocal(name) match
          case Some((tpe, localIdx)) =>
            val opCode = opcodeFor(tpe, Opcodes.ILOAD, Opcodes.ALOAD)
            mv.visitVarInsn(opCode, localIdx)
          case None =>
            mv.visitFieldInsn(Opcodes.GETSTATIC, GenFilesNames.constantsFileName, name.stringId, descriptorForType(varRef.getType))
      }

      // typechecker ensures that callee is a VariableRef
      case Call(VariableRef(name), args) => {

        // will be used as a callback when generating built-in functions
        val generateArgs: () => Unit = { () =>
          for arg <- args do {
            generateCode(arg, ctx)
          }
        }

        if (BuiltInFunctions.builtInFunctions.contains(name)) {
          name match {
            case BuiltInFunctions.print => generatePrintCall(generateArgs)
            case BuiltInFunctions.intToString => generateIntToString(generateArgs)
            case BuiltInFunctions.doubleToString => generateDoubleToString(generateArgs)
            case BuiltInFunctions.boolToString => generateBoolToString(generateArgs)
            case BuiltInFunctions.charToString => generateCharToString(generateArgs)
            case BuiltInFunctions.toCharArray => generateStringToCharArray(generateArgs)
            case _ => shouldNotHappen()
          }
        } else {
          generateArgs()
          val descriptor = descriptorForFunc(analysisContext.functions.apply(name))
          mv.visitMethodInsn(Opcodes.INVOKESTATIC, outputName, name.stringId, descriptor, false)
        }
      }

      case Indexing(indexed, arg) =>
        generateCode(indexed, ctx)
        generateCode(arg, ctx)
        val elemType = indexed.getType.asInstanceOf[ArrayType].elemType
        val opcode = opcodeFor(elemType, Opcodes.IALOAD, Opcodes.AALOAD)
        mv.visitInsn(opcode)

      case ArrayInit(elemType, size) =>
        generateCode(size, ctx)
        elemType match {
          case _: (PrimitiveType.StringType.type | StructType | ArrayType | UnionType) =>
            mv.visitTypeInsn(Opcodes.ANEWARRAY, internalNameOf(elemType))
          case _: Types.PrimitiveType =>
            val elemTypeCode = convertToAsmTypeCode(elemType).get
            mv.visitIntInsn(Opcodes.NEWARRAY, elemTypeCode)
          case Types.UndefinedType => shouldNotHappen()
        }

      case StructInit(structName, args, modifiable) =>
        val structType = StructType(structName, modifiable)
        val tmpVarIdx = ctx.currLocalIdx
        val tempLocalName = BackendGeneratedVarId(tmpVarIdx)
        ctx.addLocal(tempLocalName, structType)
        val storeTmpOpcode = opcodeFor(structType, Opcodes.ISTORE, Opcodes.ASTORE)
        mv.visitTypeInsn(Opcodes.NEW, structName.stringId)
        mv.visitInsn(Opcodes.DUP)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structName.stringId, "<init>", "()V", false)
        mv.visitVarInsn(storeTmpOpcode, tmpVarIdx)
        val structSig = analysisContext.structs.apply(structName)
        val loadTmpOpcode = opcodeFor(structType, Opcodes.ILOAD, Opcodes.ALOAD)
        for (arg, (paramName, fieldInfo)) <- args.zip(structSig.fields) do {
          mv.visitVarInsn(loadTmpOpcode, tmpVarIdx)
          generateCode(arg, ctx)
          mv.visitFieldInsn(Opcodes.PUTFIELD, structName.stringId, paramName.stringId, descriptorForType(fieldInfo.tpe))
        }
        mv.visitVarInsn(loadTmpOpcode, tmpVarIdx)

      case UnaryOp(operator, operand) =>
        generateCode(operand, ctx)
        operator match {
          case Minus if operand.getType == IntType =>
            mv.visitInsn(Opcodes.INEG)
          case Minus if operand.getType == DoubleType =>
            mv.visitInsn(Opcodes.DNEG)
          case Sharp if operand.getType.isInstanceOf[ArrayType] =>
            mv.visitInsn(Opcodes.ARRAYLENGTH)
          case Sharp if operand.getType == StringType =>
            mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, stringTypeStr, "length", "()I", false)
          case _ => throw new AssertionError(s"unexpected $operator in code generation")
        }

      case BinaryOp(lhs, operator, rhs) => {
        generateCode(lhs, ctx)
        generateCode(rhs, ctx)
        val tpe = lhs.getType
        if (operator == Equality && tpe == DoubleType) {
          /*
           *   if (lhs compare rhs) == 0 goto trueLabel:
           *   push false
           *   goto endLabel
           * trueLabel:
           *   push true
           * endLabel:
           */
          val trueLabel = new Label()
          val endLabel = new Label()
          mv.visitInsn(Opcodes.DCMPL)
          mv.visitJumpInsn(Opcodes.IFEQ, trueLabel)
          mv.visitInsn(Opcodes.ICONST_0)
          mv.visitJumpInsn(Opcodes.GOTO, endLabel)
          mv.visitLabel(trueLabel)
          mv.visitInsn(Opcodes.ICONST_1)
          mv.visitLabel(endLabel)
        } else if (operator == Equality) {
          /*
           *   if lhs == rhs goto trueLabel:
           *   push false
           *   goto endLabel
           * trueLabel:
           *   push true
           * endLabel:
           */
          val trueLabel = new Label()
          val endLabel = new Label()
          val opcode = if tpe.isInstanceOf[StructType] then Opcodes.IF_ACMPEQ else Opcodes.IF_ICMPEQ
          mv.visitJumpInsn(opcode, trueLabel)
          mv.visitInsn(Opcodes.ICONST_0)
          mv.visitJumpInsn(Opcodes.GOTO, endLabel)
          mv.visitLabel(trueLabel)
          mv.visitInsn(Opcodes.ICONST_1)
          mv.visitLabel(endLabel)
        } else if (operator == LessThan && tpe == IntType) {
          /*
           *   if_icmplt trueLabel  (goto trueLabel if lhs < rhs)
           *   push false
           *   goto endLabel
           * trueLabel:
           *   push true
           * endLabel:
           */
          val trueLabel = new Label()
          val endLabel = new Label()
          mv.visitJumpInsn(Opcodes.IF_ICMPLT, trueLabel)
          mv.visitInsn(Opcodes.ICONST_0)
          mv.visitJumpInsn(Opcodes.GOTO, endLabel)
          mv.visitLabel(trueLabel)
          mv.visitInsn(Opcodes.ICONST_1)
          mv.visitLabel(endLabel)
        } else if (operator == LessThan && tpe == DoubleType) {
          /*
           *   dcmpg  (returns -1 if lhs < rhs)
           *   if < 0 goto trueLabel
           *   push false
           *   goto endLabel
           * trueLabel:
           *   push true
           * endLabel:
           */
          mv.visitInsn(Opcodes.DCMPG)
          val trueLabel = new Label()
          val endLabel = new Label()
          mv.visitJumpInsn(Opcodes.IFLT, trueLabel)
          mv.visitInsn(Opcodes.ICONST_0)
          mv.visitJumpInsn(Opcodes.GOTO, endLabel)
          mv.visitLabel(trueLabel)
          mv.visitInsn(Opcodes.ICONST_1)
          mv.visitLabel(endLabel)
        } else if (operator == Plus && tpe == StringType) {
          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/String", "concat",
            "(Ljava/lang/String;)Ljava/lang/String;", false)
        } else {
          val intOpcode = operator match {
            case Plus => Opcodes.IADD
            case Minus => Opcodes.ISUB
            case Times => Opcodes.IMUL
            case Div => Opcodes.IDIV
            case Modulo => Opcodes.IREM
            case _ => throw new AssertionError(s"unexpected $operator in backend")
          }
          val opcode = opcodeFor(tpe, intOpcode, shouldNotHappen())
          mv.visitInsn(opcode)
        }
      }

      case Select(lhs, selected) =>
        generateCode(lhs, ctx)
        val structName = lhs.getType.asInstanceOf[StructType].typeName
        val fieldInfo = analysisContext.structs.apply(structName).fields.apply(selected)
        if (ctx.structs.apply(structName).isInterface) {
          val getterDescriptor = descriptorForFunc(FunctionSignature(selected, List.empty, fieldInfo.tpe))
          mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, structName.stringId, selected.stringId, getterDescriptor, true)
        } else {
          mv.visitFieldInsn(Opcodes.GETFIELD, structName.stringId, selected.stringId, descriptorForType(fieldInfo.tpe))
        }

      case VarAssig(lhs, rhs) =>
        lhs match {

          // x = ...
          case VariableRef(name) =>
            generateCode(rhs, ctx)
            val (varType, varIdx) = ctx.getLocal(name).get // cannot be a constant since it is an assignment
            val opcode = opcodeFor(varType, Opcodes.ISTORE, Opcodes.ASTORE)
            mv.visitVarInsn(opcode, varIdx)

          // x[y] = ...
          case Indexing(indexed, arg) =>
            generateCode(indexed, ctx)
            generateCode(arg, ctx)
            generateCode(rhs, ctx)
            val elemType = indexed.getType.asInstanceOf[ArrayType].elemType
            mv.visitInsn(opcodeFor(elemType, Opcodes.IASTORE, Opcodes.AASTORE))

          // x.y = ...
          case Select(ownerStruct, fieldName) =>
            generateCode(ownerStruct, ctx)
            generateCode(rhs, ctx)
            val ownerTypeName = ownerStruct.getType.asInstanceOf[StructType].typeName
            val fieldType = ctx.structs(ownerTypeName).fields(fieldName).tpe
            if (ctx.structs.apply(ownerTypeName).isInterface) {
              val setterSig = FunctionSignature(fieldName, List(fieldType), PrimitiveType.VoidType)
              val setterDescriptor = descriptorForFunc(setterSig)
              mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, ownerTypeName.stringId, fieldName.stringId, setterDescriptor, true)
            } else {
              mv.visitFieldInsn(Opcodes.PUTFIELD, ownerTypeName.stringId, fieldName.stringId, descriptorForType(fieldType))
            }

          case _ => shouldNotHappen()
        }

      case IfThenElse(cond, thenBr, elseBrOpt) =>
        /*
         *   if !cond goto falseLabel
         *   <thenBr>
         *   goto endLabel
         * falseLabel:
         *   <elseBr>
         * endLabel:
         */
        generateIfThenElse(ctx, cond, thenBr, elseBrOpt)

      case Ternary(cond, thenBr, elseBr) =>
        generateIfThenElse(ctx, cond, thenBr, Some(elseBr))

      case WhileLoop(cond, body) =>
        /*
         * loopLabel:
         *   if !cond goto endLabel
         *   <body>
         *   goto loopLabel
         * endLabel:
         */
        val loopLabel = new Label()
        val endLabel = new Label()
        mv.visitLabel(loopLabel)
        generateCode(cond, ctx)
        mv.visitJumpInsn(Opcodes.IFLE, endLabel)
        generateCode(body, ctx)
        mv.visitJumpInsn(Opcodes.GOTO, loopLabel)
        mv.visitLabel(endLabel)

      case ReturnStat(Some(value)) =>
        generateCode(value, ctx)
        val opcode = opcodeFor(value.getType, Opcodes.IRETURN, Opcodes.ARETURN)
        mv.visitInsn(opcode)

      case ReturnStat(None) =>
        mv.visitInsn(Opcodes.RETURN)

      case Cast(expr, tpe) => {
        generateCode(expr, ctx)
        if (!expr.getType.subtypeOf(tpe)){
          TypeConversion.conversionFor(expr.getType, tpe) match
            case Some(TypeConversion.Int2Double) => mv.visitInsn(Opcodes.I2D)
            case Some(TypeConversion.Double2Int) => mv.visitInsn(Opcodes.D2I)
            case Some(TypeConversion.IntToChar) => mv.visitInsn(Opcodes.I2C)
            case Some(TypeConversion.CharToInt) => ()
            case None => mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(tpe))
        }
      }

      case PanicStat(msg) =>
        // throw an exception
        mv.visitTypeInsn(NEW, "java/lang/RuntimeException")
        mv.visitInsn(DUP)
        generateCode(msg, ctx)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/RuntimeException",
          "<init>", s"(L$stringTypeStr;)V", false)
        mv.visitInsn(Opcodes.ATHROW)

      case other => throw new AssertionError(s"unexpected in backend: ${other.getClass}")
    }
  }

  private def generateSequence(ctx: CodeGenerationContext, stats: List[Statement], optFinalExpr: Option[Expr])
                              (using Map[TypeIdentifier, StructSignature])
                              (implicit mv: MethodVisitor, outputName: String): Unit = {
    val newCtx = ctx.withNewLocalsFrame
    for stat <- stats do {
      generateCode(stat, newCtx)
      // if unused value put on the stack then drop it
      stat match {
        case expr: Expr if expr.getType != VoidType => mv.visitInsn(Opcodes.POP)
        case _ => ()
      }
    }
    optFinalExpr.foreach { finalExpr =>
      generateCode(finalExpr, newCtx)
      // do not drop that value since it is the resulting value of the sequence
    }
  }

  private def generateIfThenElse(
                                  ctx: CodeGenerationContext,
                                  cond: Expr,
                                  thenBr: Statement,
                                  elseBrOpt: Option[Statement]
                                )
                                (using Map[TypeIdentifier, StructSignature])
                                (implicit mv: MethodVisitor, outputName: String): Unit = {
    /*
    * if !cond then goto falseLabel
    *    <then branch>
    *    goto endLabel
    * falseLabel:
    *    <elseBranch>
    * endLabel:
    */
    generateCode(cond, ctx)
    val falseLabel = new Label()
    val endLabel = new Label()
    mv.visitJumpInsn(Opcodes.IFLE, falseLabel)
    generateCode(thenBr, ctx)
    mv.visitJumpInsn(Opcodes.GOTO, endLabel)
    mv.visitLabel(falseLabel)
    elseBrOpt.foreach(generateCode(_, ctx))
    mv.visitLabel(endLabel)
  }

  private def shouldNotHappen(): Nothing = assert(false)

}

object Backend {

  private val objectTypeStr = "java/lang/Object"
  private val stringTypeStr = "java/lang/String"

  /**
   * Each mode injects specific behavior into the Backend where it differs between binary generation and textual bytecode generation
   */
  sealed trait Mode[V <: ClassVisitor] {
    val extension: String

    def createVisitor(path: Path): V

    def terminate(visitor: V, path: Path, errorReporter: ErrorReporter): Unit

    final def withExtension(filename: String): String = s"$filename.$extension"
  }

  /**
   * Use this mode to generate JVM binaries (.class)
   */
  case object BinaryMode extends Mode[ClassWriter] {

    override val extension: String = FileExtensions.binary

    override def createVisitor(path: Path): ClassWriter = {
      new ClassWriter(ClassWriter.COMPUTE_FRAMES){
        override def getCommonSuperClass(type1: String, type2: String): String = objectTypeStr
      }
    }

    override def terminate(visitor: ClassWriter, path: Path, errorReporter: ErrorReporter): Unit = {
      val file = path.toFile
      file.createNewFile()
      val status = Using(new FileOutputStream(file)) { fileOutputStream =>
        fileOutputStream.write(visitor.toByteArray)
        fileOutputStream.flush()
      }
      status match
        case Success(_) => ()
        case Failure(exception) =>
          errorReporter.push(Err(CodeGeneration, exception.getMessage, None))
    }
  }

  /**
   * Use this mode to generate textual JVM bytecode files
   */
  case object AssemblyMode extends Mode[TraceClassVisitor] {

    override val extension: String = FileExtensions.assembly

    override def createVisitor(path: Path): TraceClassVisitor = {
      val file = path.toFile
      file.createNewFile()
      new TraceClassVisitor(new PrintWriter(file))
    }

    override def terminate(visitor: TraceClassVisitor, path: Path, errorReporter: ErrorReporter): Unit = {
      // nothing to do
      Success(())
    }
  }

}
