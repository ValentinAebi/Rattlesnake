package compiler.backend

import com.sun.org.apache.bcel.internal.generic.{ALOAD, INVOKESPECIAL}
import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FileExtensions}
import compiler.CompilationStep.CodeGeneration
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.backend.TypesConverter.{convertToAsmType, convertToAsmTypeCode, opcodeFor}
import lang.Operator.*
import lang.Types.{ArrayType, PrimitiveType, StructType}
import lang.Types.PrimitiveType.StringType
import lang.{BuiltInFunctions, Types}
import Types.PrimitiveType.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.objectweb.asm.*
import org.objectweb.asm

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.immutable.Nil
import scala.util.{Failure, Success, Try, Using}

final class Backend[V <: ClassVisitor](
                                        mode: Backend.Mode[V],
                                        errorReporter: ErrorReporter,
                                        outputDirBase: Path,
                                        optName: Option[String] = None
                                      ) extends CompilerStep[(List[Source], AnalysisContext), Unit] {

  private val javaVersion = V17
  private val objectTypeStr = "java/lang/Object"
  private val stringTypeStr = "java/lang/String"

  override def apply(input: (List[Source], AnalysisContext)): Unit = {
    val (sources, analysisContext) = input
    if (sources.isEmpty) {
      errorReporter.push(new CompilationError(CodeGeneration, "nothing to write: no sources", None))
    }
    else {

      val outputDir = outputDirBase.resolve("out")
      val filename = Path.of(sources.head.getName).getFileName.toString.takeWhile(_ != '.')
      val outputName = optName.getOrElse(filename).withHeadUppercase + "_core"

      val functionsBuilder = List.newBuilder[FunDef]
      val structsBuilder = List.newBuilder[StructDef]
      for src <- sources; df <- src.defs do {
        df match
          case funDef: FunDef => functionsBuilder.addOne(funDef)
          case structDef: StructDef => structsBuilder.addOne(structDef)
      }
      val functions = functionsBuilder.result()
      val structs = structsBuilder.result()

      for struct <- structs do {
        val structFilePath = outputDir.resolve(mode.withExtension(struct.structName))
        val cv = mode.createVisitor(structFilePath)
        cv.visit(javaVersion, ACC_PUBLIC, struct.structName, null, objectTypeStr, null)
        generateStruct(struct, cv)
        cv.visitEnd()
      }

      Files.createDirectories(outputDir)
      val coreFilePath = outputDir.resolve(mode.withExtension(outputName))
      val cv: V = mode.createVisitor(coreFilePath)
      cv.visit(V17, ACC_PUBLIC, outputName, null, objectTypeStr, null)
      for function <- functions do {
        val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, function.funName, descriptorForFunc(function.signature), null, null)
        generateFunction(function, mv, analysisContext, outputName)
      }
      cv.visitEnd()
      mode.terminate(cv, coreFilePath) match
        case Success(_) => ()
        case Failure(exception) =>
          errorReporter.push(new CompilationError(CodeGeneration, exception.getMessage, None))
    }
    errorReporter.displayAndTerminateIfErrors()
  }

  private def generateStruct(structDef: StructDef, cv: ClassVisitor): Unit = {
    for field <- structDef.fields do {
      cv.visitField(ACC_PUBLIC, field.paramName, descriptorForType(field.tpe), null, null)
    }
  }

  private def generateFunction(funDef: FunDef, mv: MethodVisitor, analysisContext: AnalysisContext, outputName: String): Unit = {
    val ctx = CodeGenerationContext.from(analysisContext)
    for param <- funDef.params do {
      ctx.addLocal(param.paramName, param.tpe)
    }
    mv.visitCode()
    generateCode(funDef.body, ctx)(mv, outputName)
    if (ctx.analysisContext.functions.apply(funDef.funName).retType == VoidType) {
      mv.visitInsn(Opcodes.RETURN)
    }
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  private def generateCode(ast: Ast, ctx: CodeGenerationContext)(implicit mv: MethodVisitor, outputName: String): Unit = {
    val analysisContext = ctx.analysisContext
    // TODO
    ast match {

      case Block(stats) =>
        val newCtx = ctx.withNewLocalsFrame
        for stat <- stats do {
          generateCode(stat, newCtx)
        }

      case VarDef(varName, tpeOpt, rhs) =>
        generateCode(rhs, ctx)
        val opcode = opcodeFor(rhs.getType, Opcodes.ISTORE, Opcodes.ASTORE)
        mv.visitVarInsn(opcode, ctx.currLocalIdx)
        ctx.addLocal(varName, tpeOpt.get)

      case IntLit(value) => mv.visitLdcInsn(value)
      case DoubleLit(value) => mv.visitLdcInsn(value)
      case CharLit(value) => mv.visitLdcInsn(value)
      case BoolLit(value) => mv.visitLdcInsn(if value then 1 else 0)
      case StringLit(value) => mv.visitLdcInsn(value)

      case VariableRef(name) =>
        val (tpe, localIdx) = ctx.getLocal(name)
        val opCode = opcodeFor(tpe, Opcodes.ILOAD, Opcodes.ALOAD)
        mv.visitVarInsn(opCode, localIdx)

      // typechecker ensures that callee is a VariableRef
      case Call(VariableRef(name), args) => {

        val generateArgs: () => Unit = { () =>
          for arg <- args do {
            generateCode(arg, ctx)
          }
        }

        if (BuiltInFunctions.builtInFunctions.contains(name)) {
          import BuiltinFunctionsImpl.*
          name match {
            case BuiltInFunctions.print => generatePrintCall(generateArgs)
            case BuiltInFunctions.intToString => generateIntToString(generateArgs)
            case BuiltInFunctions.doubleToString => generateDoubleToString(generateArgs)
            case BuiltInFunctions.boolToString => generateBoolToString(generateArgs)
            case BuiltInFunctions.charToString => generateCharToString(generateArgs)
            case _ => shouldNotHappen()
          }
        } else {
          generateArgs()
          val descriptor = descriptorForFunc(analysisContext.functions.apply(name))
          mv.visitMethodInsn(Opcodes.INVOKESTATIC, outputName, name, descriptor, false)
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
          case PrimitiveType.StringType =>
            mv.visitTypeInsn(Opcodes.ANEWARRAY, stringTypeStr)
          case StructType(typeName) =>
            mv.visitTypeInsn(Opcodes.ANEWARRAY, typeName)
          case ArrayType(elemType) =>
            mv.visitTypeInsn(Opcodes.ANEWARRAY, ???) // TODO multidim arrays
          case _: Types.PrimitiveType =>
            val elemTypeCode = convertToAsmTypeCode(elemType).get
            mv.visitIntInsn(Opcodes.NEWARRAY, elemTypeCode)
        }

      case StructInit(structName, args) =>
        mv.visitTypeInsn(Opcodes.NEW, structName)
        mv.visitInsn(Opcodes.DUP)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, structName, "<init>", "()V", false)
        val structSig = analysisContext.structs.apply(structName)
        for (arg, (paramName, tpe)) <- args.zip(structSig.fields) do {
          generateCode(arg, ctx)
          mv.visitFieldInsn(Opcodes.PUTFIELD, structName, paramName, descriptorForType(tpe))
        }

      case UnaryOp(operator, operand) =>
        generateCode(operand, ctx)
        operator match {
          case Sharp => mv.visitInsn(Opcodes.ARRAYLENGTH)
          case _ => shouldNotHappen()
        }

      case BinaryOp(lhs, operator, rhs) =>
        generateCode(lhs, ctx)
        generateCode(rhs, ctx)
        val tpe = lhs.getType
        if (operator == Equality) {
          /*
           *   if lhs == rhs goto trueLabel:
           *   push false
           *   goto endLabel
           * trueLabel:
           *   push true
           * endLabel:
           */
          val opcode = opcodeFor(tpe, Opcodes.IF_ICMPEQ, Opcodes.IF_ACMPEQ)
          val trueLabel = new Label()
          val endLabel = new Label()
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
        } else {
          val intOpcode = operator match {
            case Plus => Opcodes.IADD
            case Minus => Opcodes.ISUB
            case Times => Opcodes.IMUL
            case Div => Opcodes.IDIV
            case Modulo => Opcodes.IREM
            case And => Opcodes.IAND
            case Or => Opcodes.IOR
            case _ => throw new IllegalStateException(s"unexpected $operator in backend")
          }
          val opcode = opcodeFor(tpe, intOpcode, shouldNotHappen())
          mv.visitInsn(opcode)
        }

      case Select(lhs, selected) =>
        generateCode(lhs, ctx)
        val structName = lhs.getType.asInstanceOf[StructType].typeName
        val selectedType = analysisContext.structs.apply(structName).fields.apply(selected)
        mv.visitFieldInsn(Opcodes.GETFIELD, structName, selected, descriptorForType(selectedType))

      case VarAssig(lhs, rhs) =>
        lhs match {
          case VariableRef(name) =>
            generateCode(rhs, ctx)
            val (varType, varIdx) = ctx.getLocal(name)
            val opcode = opcodeFor(varType, Opcodes.ISTORE, Opcodes.ASTORE)
            mv.visitVarInsn(opcode, varIdx)
          case Indexing(indexed, arg) =>
            generateCode(indexed, ctx)
            generateCode(arg, ctx)
            generateCode(rhs, ctx)
            val elemType = indexed.getType.asInstanceOf[ArrayType].elemType
            mv.visitInsn(opcodeFor(elemType, Opcodes.IASTORE, Opcodes.AASTORE))
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
        generateCode(cond, ctx)
        val falseLabel = new Label()
        val endLabel = new Label()
        mv.visitJumpInsn(Opcodes.IFLE, falseLabel)
        generateCode(thenBr, ctx)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
        mv.visitLabel(falseLabel)
        elseBrOpt.foreach(generateCode(_, ctx))
        mv.visitLabel(endLabel)

      case Ternary(cond, thenBr, elseBr) =>
        generateCode(IfThenElse(cond, thenBr, Some(elseBr)), ctx)

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

      case ReturnStat(value) =>
        generateCode(value, ctx)
        val opcode = opcodeFor(value.getType, Opcodes.IRETURN, Opcodes.ARETURN)
        mv.visitInsn(opcode)

      case PanicStat(msg) =>
        mv.visitTypeInsn(NEW, "java/lang/RuntimeException")
        mv.visitInsn(DUP)
        generateCode(msg, ctx)
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/RuntimeException",
          "<init>", s"(L$stringTypeStr;)V", false)
        mv.visitInsn(Opcodes.ATHROW)

      case other => throw new IllegalStateException(s"unexpected in backend: ${other.getClass}")
    }
  }

  extension (str: String) private def withHeadUppercase: String = {
    if str.isEmpty then str
    else str.head.toUpper +: str.tail
  }

  private def shouldNotHappen(): Nothing = assert(false)

}

object Backend {

  sealed trait Mode[V <: ClassVisitor] {
    val extension: String

    def createVisitor(path: Path): V

    def terminate(visitor: V, path: Path): Try[Unit]

    final def withExtension(filename: String): String = s"$filename.$extension"
  }

  case object BinaryMode extends Mode[ClassWriter] {

    override val extension: String = FileExtensions.binary

    override def createVisitor(path: Path): ClassWriter = {
      new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    }

    override def terminate(visitor: ClassWriter, path: Path): Try[Unit] = {
      val file = path.toFile
      file.createNewFile()
      Using(new FileOutputStream(file)) { fileOutputStream =>
        fileOutputStream.write(visitor.toByteArray)
        fileOutputStream.flush()
      }
    }
  }

  case object AssemblyMode extends Mode[TraceClassVisitor] {

    override val extension: String = FileExtensions.assembly

    override def createVisitor(path: Path): TraceClassVisitor = {
      val file = path.toFile
      file.createNewFile()
      new TraceClassVisitor(new PrintWriter(file))
    }

    override def terminate(visitor: TraceClassVisitor, path: Path): Try[Unit] = {
      // nothing to do
      Success(())
    }
  }

}
