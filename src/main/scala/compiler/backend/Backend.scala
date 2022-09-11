package compiler.backend

import com.sun.org.apache.bcel.internal.generic.{ALOAD, INVOKESPECIAL}
import compiler.Errors.{CompilationError, ErrorReporter}
import compiler.irs.Asts.*
import compiler.{AnalysisContext, CompilerStep, FileExtensions}
import compiler.CompilationStep.CodeGeneration
import compiler.backend.DescriptorsCreator.{descriptorForFunc, descriptorForType}
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.util.{Textifier, TraceClassVisitor}
import org.objectweb.asm.*

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
  private val superType = "java/lang/Object"

  override def apply(input: (List[Source], AnalysisContext)): Unit = {
    val (sources, ctx) = input
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
        cv.visit(javaVersion, ACC_PUBLIC, struct.structName, null, superType, null)
        generateStruct(struct, cv)
        cv.visitEnd()
      }

      Files.createDirectories(outputDir)
      val coreFilePath = outputDir.resolve(mode.withExtension(outputName))
      val cv: V = mode.createVisitor(coreFilePath)
      cv.visit(V17, ACC_PUBLIC, outputName, null, superType, null)
      for function <- functions do {
        val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, function.funName, descriptorForFunc(function.signature), null, null)
        generateFunction(function, mv)
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

  private def generateFunction(funDef: FunDef, mv: MethodVisitor): Unit = {
    mv.visitCode()

    // TODO

    mv.visitEnd()
  }

//  private def generateCode(ast: Ast, ctx: AnalysisContext): Unit = {
//    ast match {
//
//      case Source(defs) =>
//        for df <- defs do {
//          generateCode(df, ctx.copyWithoutLocals)
//        }
//
//      case Block(stats) =>
//        val newCtx = ctx.copied
//        for stat <- stats do {
//          generateCode(stat, newCtx)
//        }
//
//      case FunDef(funName, params, optRetType, body) =>
//
//
//      case StructDef(_, fields) =>
//
//      case Param(_, _) =>
//
//      case ValDef(_, _, rhs) =>
//
//      case VarDef(_, _, rhs) =>
//
//      case _: Literal =>
//
//      case VariableRef(_) =>
//
//      case Call(callee, args) =>
//
//      case Indexing(indexed, arg) =>
//
//      case ArrayInit(_, size) =>
//
//      case StructInit(_, args) =>
//
//      case UnaryOp(_, operand) =>
//
//      case BinaryOp(lhs, _, rhs) =>
//
//      case Select(lhs, _) =>
//
//      case VarAssig(lhs, rhs) =>
//
//      case VarModif(lhs, rhs, _) =>
//
//      case IfThenElse(cond, thenBr, elseBrOpt) =>
//
//      case WhileLoop(cond, body) =>
//
//      case ForLoop(initStats, cond, stepStats, body) =>
//
//      case ReturnStat(value) =>
//
//      case PanicStat(msg) =>
//
//    }
//    ??? // TODO
//  }

  extension(str: String) private def withHeadUppercase: String = {
    if str.isEmpty then str
    else str.head.toUpper +: str.tail
  }

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
      new ClassWriter(ClassWriter.COMPUTE_MAXS)
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
