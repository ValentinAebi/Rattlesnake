package compiler.backend

import com.sun.org.apache.bcel.internal.generic.{ALOAD, INVOKESPECIAL}
import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FileExtensions}
import compiler.CompilationStep.CodeGeneration
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import compiler.backend.TypesConverter.{convertToAsmType, convertToAsmTypeCode, opcodeFor}
import lang.Types.{ArrayType, PrimitiveType, StructType}
import lang.Types.PrimitiveType.StringType
import lang.{BuiltInFunctions, Types}
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
    mv.visitCode()
    generateCode(funDef.body, CodeGenerationContext.from(analysisContext))(mv, outputName)
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
        mv.visitVarInsn(Opcodes.ASTORE, ctx.currLocalIdx)
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
      case Call(VariableRef(name), args) =>
        // load arguments
        for arg <- args do {
          generateCode(arg, ctx)
        }
        if (BuiltInFunctions.builtInFunctions.contains(name)) {
          ??? // TODO generation of built in functions
        } else {
          val descriptor = descriptorForFunc(analysisContext.functions.apply(name))
          mv.visitMethodInsn(Opcodes.INVOKESTATIC, outputName, name, descriptor, false)
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
            mv.visitTypeInsn(Opcodes.ANEWARRAY, ???)  // TODO multidim arrays
          case _: Types.PrimitiveType =>
            val elemTypeCode = convertToAsmTypeCode(elemType).get
            mv.visitIntInsn(Opcodes.NEWARRAY, elemTypeCode)
        }

      case StructInit(structName, args) =>

      case UnaryOp(operator, operand) =>

      case BinaryOp(lhs, operator, rhs) =>

      case Select(lhs, selected) =>

      case VarAssig(lhs, rhs) =>

      case IfThenElse(cond, thenBr, elseBrOpt) =>

      case WhileLoop(cond, body) =>

      case ReturnStat(value) =>

      case PanicStat(msg) =>

      case _ => shouldNotHappen()
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
