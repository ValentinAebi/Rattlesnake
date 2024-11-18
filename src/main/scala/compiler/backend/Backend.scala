package compiler.backend

import compiler.CompilationStep.CodeGeneration
import compiler.Errors.*
import compiler.NamesForGeneratedClasses.packageInstanceName
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.backend.TypesConverter.{convertToAsmTypeCode, internalNameOf, opcodeFor}
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FileExtensions, NamesForGeneratedClasses}
import identifiers.{BackendGeneratedVarId, FunOrVarId, MeVarId, TypeIdentifier}
import lang.*
import lang.Captures.CaptureSet
import lang.Operator.*
import lang.SubtypeRelation.subtypeOf
import lang.Types.PrimitiveType.*
import lang.Types.{ArrayType, NamedType, PrimitiveType, UnionType}
import org.objectweb.asm
import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.util.TraceClassVisitor

import java.io.{FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.{Failure, Success, Using}

/**
 * Generates the output files: 1 core file, containing the program, and 1 file per struct
 *
 * @param mode              cf nested class [[Backend.Mode]]
 * @param outputDirBase     output directory path (will be extended with `/out`)
 * @tparam V depends on the mode
 */
final class Backend[V <: ClassVisitor](
                                        mode: Backend.Mode[V],
                                        errorReporter: ErrorReporter,
                                        outputDirBase: Path,
                                        javaVersionCode: Int
                                      ) extends CompilerStep[(List[Source], AnalysisContext), List[Path]] {

  import Backend.*

  override def apply(input: (List[Source], AnalysisContext)): List[Path] = {
    val (sources, analysisContext) = input
    if (sources.isEmpty) {
      errorReporter.pushFatal(Fatal(CodeGeneration, "nothing to write: no sources", None))
    } else {
      val modulesAndPackagesBuilder = List.newBuilder[TypeDefTree]
      val structsBuilder = List.newBuilder[StructDef]
      val constsBuilder = List.newBuilder[ConstDef]
      for src <- sources; df <- src.defs do {
        df match
          case modOrPkg: ModuleOrPackageDefTree => modulesAndPackagesBuilder.addOne(modOrPkg)
          case structDef: StructDef => structsBuilder.addOne(structDef)
          case constDef: ConstDef => constsBuilder.addOne(constDef)
      }

      // sort the structs so that no class is loaded before its super-interfaces
      val modulesAndPackages = modulesAndPackagesBuilder.result()
      val structs = sortStructs(structsBuilder.result())
      val consts = constsBuilder.result()

      // create output directory if it does not already exist
      val outputDir = outputDirBase.resolve("out")
      Files.createDirectories(outputDir)

      given AnalysisContext = analysisContext

      val generatedClassFiles = mutable.ListBuffer.empty[Path]

      generateTypes(modulesAndPackages, outputDir, generatedClassFiles)
      generateTypes(structs, outputDir, generatedClassFiles)

      if (consts.nonEmpty){
        val constantsFilePath = outputDir.resolve(mode.withExtension(NamesForGeneratedClasses.constantsClassName))
        generateConstantsFile(consts, constantsFilePath)
        generatedClassFiles.addOne(constantsFilePath)
      }

      errorReporter.displayAndTerminateIfErrors()
      generatedClassFiles.toList
    }

  }

  /**
   * Topological sort on the subtyping relation: each supertype appears before its subtypes
   */
  private def sortStructs(structs: List[StructDef]): List[StructDef] = {
    val remStructs = mutable.Queue.from(structs)
    val sortedList = new mutable.LinkedHashMap[TypeIdentifier, StructDef]
    while (remStructs.nonEmpty) {
      // get is safe here because the subtyping relation admits no cycles (checked by ContextCreation phase)
      val curr = remStructs.removeFirst(_.directSupertypes.forall(sortedList.contains)).get
      sortedList.addOne(curr.structName -> curr)
    }
    sortedList.toList.map(_._2)
  }

  private def generateConstantsFile(consts: List[ConstDef], constantsFilePath: Path)
                                   (using AnalysisContext): Unit = {
    val ccv: V = mode.createVisitor(constantsFilePath)
    ccv.visit(javaVersionCode, ACC_PUBLIC, NamesForGeneratedClasses.constantsClassName, null, objectTypeStr, null)
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

  private def generateModuleOrPackageFile(modOrPkg: ModuleOrPackageDefTree, path: Path)(using ctx: AnalysisContext): Unit = {
    val cv: V = mode.createVisitor(path)
    cv.visit(javaVersionCode, ACC_PUBLIC | ACC_FINAL, modOrPkg.name.stringId, null, objectTypeStr, null)
    addConstructor(cv)
    val pkgTypeDescr = descriptorForType(NamedType(modOrPkg.name, CaptureSet.empty))
    if (modOrPkg.isInstanceOf[PackageDef]) {
      addInstanceFieldAndInitializer(modOrPkg, cv, pkgTypeDescr)
    }
    for (varId, moduleId) <- ctx.resolveType(modOrPkg.name).get.asInstanceOf[ModuleOrPackageSignature].paramImports do {
      cv.visitField(ACC_PRIVATE, varId.stringId, descriptorForType(NamedType(moduleId, CaptureSet.empty)), null, null)
    }
    for func <- modOrPkg.functions do {
      val desc = descriptorForFunc(func.signature)
      val mv = cv.visitMethod(ACC_PUBLIC, func.funName.stringId, desc, null, null)
      addFunction(modOrPkg.name, func, mv, ctx)
    }
    cv.visitEnd()
    mode.terminate(cv, path, errorReporter)
  }

  private def addInstanceFieldAndInitializer(modOrPkg: ModuleOrPackageDefTree, cv: V, pkgTypeDescr: String): Unit = {
    val pkgName = modOrPkg.name.stringId
    cv.visitField(
      ACC_PUBLIC | ACC_STATIC,
      packageInstanceName,
      pkgTypeDescr,
      null,
      null
    )
    val mv = cv.visitMethod(
      ACC_PRIVATE | ACC_STATIC,
      "<clinit>",
      "()V",
      null,
      null
    )
    mv.visitCode()
    mv.visitTypeInsn(Opcodes.NEW, pkgName)
    mv.visitInsn(Opcodes.DUP)
    mv.visitMethodInsn(Opcodes.INVOKESPECIAL, pkgName, "<init>", "()V", false)
    mv.visitFieldInsn(Opcodes.PUTSTATIC, pkgName, packageInstanceName, pkgTypeDescr)
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  private def addFunction(owner: TypeIdentifier, funDef: FunDef, mv: MethodVisitor, analysisContext: AnalysisContext): Unit = {
    val ctx = CodeGenerationContext.from(analysisContext, owner)
    ctx.addLocal(MeVarId, NamedType(owner, CaptureSet.empty))
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
    generateCode(funDef.body, ctx)(using mv)
    if (ctx.resolveFunc(owner, funDef.funName).getOrThrow().retType == VoidType) {
      mv.visitInsn(Opcodes.RETURN)
    }
    mv.visitMaxs(0, 0) // parameters are ignored because mode is COMPUTE_FRAMES
    mv.visitEnd()
  }

  private def generateTypes(
                             types: List[TypeDefTree],
                             outputDir: Path,
                             generatedFilesPaths: mutable.ListBuffer[Path]
                           )(using AnalysisContext): Unit = {
    for tpe <- types do {
      val classFilePath = outputDir.resolve(mode.withExtension(tpe.name.stringId))
      tpe match {
        case struct: StructDef =>
          val interfaces = struct.directSupertypes.map(_.stringId).toArray
          generateStruct(struct, interfaces, classFilePath)
        case modOrPkg: ModuleOrPackageDefTree =>
          generateModuleOrPackageFile(modOrPkg, classFilePath)
      }
      generatedFilesPaths.addOne(classFilePath)
    }
  }

  private def generateStruct(structDef: StructDef, superInterfaces: Array[String],
                             structFilePath: Path)(using ctx: AnalysisContext): Unit = {

    given Map[TypeIdentifier, StructSignature] = ctx.structs

    val structName = structDef.structName
    val sig = ctx.structs.apply(structName)
    val isInterface = sig.isInterface
    val cv = mode.createVisitor(structFilePath)
    var classMods = ACC_PUBLIC
    if (isInterface) {
      classMods |= ACC_INTERFACE
      classMods |= ACC_ABSTRACT
    } else {
      classMods |= ACC_FINAL
    }
    cv.visit(javaVersionCode, classMods, structDef.structName.stringId, null, objectTypeStr, superInterfaces)
    if (!isInterface) {
      addFields(structDef, cv)
      addConstructor(cv)
    }
    val fieldsWithAccessors = if isInterface then sig.fields.keySet else getInterfaceFieldsForStruct(structName, ctx.structs)
    for (fld <- fieldsWithAccessors) {
      val fldType = sig.fields.apply(fld).tpe
      val fieldDescr = descriptorForType(fldType)
      generateGetter(structName, fld, fldType, fieldDescr, cv, genImplementation = !isInterface)
      if (sig.fields.apply(fld).isReassignable) {
        generateSetter(structName, fld, fldType, fieldDescr, cv, genImplementation = !isInterface)
      }
    }
    cv.visitEnd()
    mode.terminate(cv, structFilePath, errorReporter)
  }

  private def generateGetter(structName: TypeIdentifier, fld: FunOrVarId, fldType: Types.Type,
                             fieldDescr: String, cv: ClassVisitor, genImplementation: Boolean)
                            (using AnalysisContext): Unit = {
    val getterDescriptor = descriptorForFunc(FunctionSignature(fld, List.empty, fldType))
    var modifiers = ACC_PUBLIC
    if (!genImplementation) {
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
                            (using AnalysisContext): Unit = {
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
                       (using AnalysisContext): Unit = {
    for field <- structDef.fields do {
      val fieldVisitor = cv.visitField(Opcodes.ACC_PUBLIC, field.paramNameOpt.get.stringId, descriptorForType(field.tpe), null, null)
      fieldVisitor.visitEnd()
    }
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
                          (using mv: MethodVisitor): Unit = {

    val analysisContext = ctx.analysisContext
    given AnalysisContext = analysisContext

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
            mv.visitFieldInsn(
              Opcodes.GETSTATIC,
              NamesForGeneratedClasses.constantsClassName,
              name.stringId,
              descriptorForType(varRef.getType)
            )
      }

      case MeRef() =>
        mv.visitIntInsn(opcodeFor(NamedType(ctx.currentModule, CaptureSet.empty), Opcodes.ILOAD, Opcodes.ALOAD), 0)

      case PackageRef(name) =>
        val packageShapeType = NamedType(name, CaptureSet.empty)
        val internalName = internalNameOf(packageShapeType)
        val descr = descriptorForType(packageShapeType)
        mv.visitFieldInsn(Opcodes.GETSTATIC, internalName, packageInstanceName, descr)

      case Call(None, funName, args) => {
        val generateArgs: () => Unit = { () =>
          for arg <- args do {
            generateCode(arg, ctx)
          }
        }
        IntrinsicsImpl.intrinsicsMap.apply(funName)(generateArgs, mv)
      }

      case Call(Some(receiver), funName, args) => {
        generateCode(receiver, ctx)
        for arg <- args do {
          generateCode(arg, ctx)
        }
        val recvInternalName = internalNameOf(receiver.getType)
        val receiverTypeName = receiver.getType.asInstanceOf[NamedType].typeName
        val funDescr = descriptorForFunc(ctx.resolveFunc(receiverTypeName, funName).getOrThrow())
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, recvInternalName, funName.stringId, funDescr, false)
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
          case _: (PrimitiveType.StringType.type | NamedType | ArrayType | UnionType) =>
            mv.visitTypeInsn(Opcodes.ANEWARRAY, internalNameOf(elemType))
          case _: Types.PrimitiveType =>
            val elemTypeCode = convertToAsmTypeCode(elemType).get
            mv.visitIntInsn(Opcodes.NEWARRAY, elemTypeCode)
          case Types.UndefinedType => shouldNotHappen()
        }

      case StructOrModuleInstantiation(tid, args) =>
        val instantiatedType = NamedType(tid, CaptureSet.empty)
        val tmpVarIdx = ctx.currLocalIdx
        val tempLocalName = BackendGeneratedVarId(tmpVarIdx)
        ctx.addLocal(tempLocalName, instantiatedType)
        val storeTmpOpcode = opcodeFor(instantiatedType, Opcodes.ISTORE, Opcodes.ASTORE)
        mv.visitTypeInsn(Opcodes.NEW, tid.stringId)
        mv.visitInsn(Opcodes.DUP)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, tid.stringId, "<init>", "()V", false)
        mv.visitVarInsn(storeTmpOpcode, tmpVarIdx)
        val sig = analysisContext.resolveType(tid).get
        val loadTmpOpcode = opcodeFor(instantiatedType, Opcodes.ILOAD, Opcodes.ALOAD)
        for (arg, (paramName, paramType)) <- args.zip(instantiationParameters(sig)) do {
          mv.visitVarInsn(loadTmpOpcode, tmpVarIdx)
          generateCode(arg, ctx)
          mv.visitFieldInsn(Opcodes.PUTFIELD, tid.stringId, paramName, descriptorForType(paramType))
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
        if (operator == Equality && tpe == StringType) {
          mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, stringTypeStr, "equals", "(Ljava/lang/Object;)Z", false)
        } else if (operator == Equality && tpe == DoubleType) {
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
          val opcode = if tpe.isInstanceOf[NamedType] then Opcodes.IF_ACMPEQ else Opcodes.IF_ICMPEQ
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
        val structName = lhs.getType.asInstanceOf[NamedType].typeName
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
            val ownerTypeName = ownerStruct.getType.asInstanceOf[NamedType].typeName
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

      case ite@IfThenElse(cond, thenBr, elseBrOpt) =>
        /*
         *   if !cond goto falseLabel
         *   <thenBr>
         *   goto endLabel
         * falseLabel:
         *   <elseBr>
         * endLabel:
         */
        generateIfThenElse(ite, ctx)

      case ternary@Ternary(cond, thenBr, elseBr) =>
        generateIfThenElse(ternary, ctx)

      case whileLoop@WhileLoop(cond, body) =>
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

      case cast@Cast(expr, tpe) => {
        generateCode(expr, ctx)
        if (!cast.isTransparentCast) {
          TypeConversion.conversionFor(expr.getType, tpe) match
            case Some(TypeConversion.Int2Double) => mv.visitInsn(Opcodes.I2D)
            case Some(TypeConversion.Double2Int) => mv.visitInsn(Opcodes.D2I)
            case Some(TypeConversion.IntToChar) => mv.visitInsn(Opcodes.I2C)
            case Some(TypeConversion.CharToInt) => ()
            case None => mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(tpe))
        }
      }

      case TypeTest(expr, tpe) => {
        generateCode(expr, ctx)
        mv.visitTypeInsn(Opcodes.INSTANCEOF, internalNameOf(tpe))
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
                              (using mv: MethodVisitor): Unit = {
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
                                  conditional: Conditional,
                                  ctx: CodeGenerationContext
                                )(using mv: MethodVisitor): Unit = {
    /*
    * if !cond then goto falseLabel
    *    <then branch>
    *    goto endLabel
    * falseLabel:
    *    <else branch>
    * endLabel:
    */
    generateCode(conditional.cond, ctx)
    val falseLabel = new Label()
    val endLabel = new Label()
    mv.visitJumpInsn(Opcodes.IFLE, falseLabel)
    generateSmartCasts(conditional.getSmartCasts, ctx)
    generateCode(conditional.thenBr, ctx)
    mv.visitJumpInsn(Opcodes.GOTO, endLabel)
    mv.visitLabel(falseLabel)
    conditional.elseBrOpt.foreach(generateCode(_, ctx))
    mv.visitLabel(endLabel)
  }

  private def generateSmartCasts(smartCasts: Map[FunOrVarId, Types.Type], ctx: CodeGenerationContext)
                                (using mv: MethodVisitor): Unit = {
    for (varId, destType) <- smartCasts do {
      // typechecker has already checked that varId is not a constant
      val (originType, varIdx) = ctx.getLocal(varId).get
      mv.visitIntInsn(Opcodes.ALOAD, varIdx)
      mv.visitTypeInsn(Opcodes.CHECKCAST, internalNameOf(destType)(using ctx.analysisContext))
      mv.visitIntInsn(Opcodes.ASTORE, varIdx)
    }
  }

  private def instantiationParameters(typeSig: TypeSignature): List[(String, Types.Type)] = typeSig match {
    case StructSignature(name, fields, directSupertypes, isInterface) =>
      fields.toList.map((id, infos) => (id.stringId, infos.tpe))
    case ModuleSignature(name, importedModules, importedPackages, importedDevices, functions) =>
      importedModules.toList.map((id, tpe) => (id.stringId, tpe))
    case _ => shouldNotHappen()
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
      new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
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
    }
  }

}
