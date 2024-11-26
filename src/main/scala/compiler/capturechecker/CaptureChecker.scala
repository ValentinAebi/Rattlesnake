package compiler.capturechecker

import compiler.analysisctx.AnalysisContext
import compiler.irs.Asts.{Ast, FunDef, Param, Source}
import compiler.irs.CaptureFlows.*
import compiler.irs.{Asts, CaptureFlows}
import compiler.pipeline.CompilerStep
import compiler.reporting.Errors.ErrorReporter
import lang.{ProgramValue, CapturableValue}
import lang.Types.PrimitiveType.{NothingType, VoidType}

import scala.annotation.tailrec
import scala.collection.mutable

final class CaptureChecker(er: ErrorReporter)
  extends CompilerStep[(List[Source], AnalysisContext), (List[Source], AnalysisContext)] {

  override def apply(input: (List[Source], AnalysisContext)): (List[Source], AnalysisContext) = {
    ???
  }

  private def compileToCaptureFlow(funDef: FunDef)(using ct: CapturesTracker): CaptureFlow = {
    val basicBlocks = mutable.ListBuffer.empty[BasicBlock]
    var currentBB = mutable.ListBuffer.empty[BBInstr]
    
    def addInstr(instr: BBInstr): Unit = currentBB.addOne(instr)
    
    def compile(ast: Ast): Option[Register] = ast match {
      case literal: Asts.Literal =>
        val register = new GenRegister
        addInstr(Assignment(register, NewValue(new ProgramValue("function constant")), literal.getPosition))
        Some(register)
      case regionCreation: Asts.RegionCreation =>
        val register = new GenRegister
        val regionValue = new ProgramValue("region")
        ct(regionValue) = CaptureSet.singletonOfRoot
        addInstr(Assignment(register, NewValue(regionValue), regionCreation.getPosition))
        Some(register)
      case Asts.VariableRef(name) =>
        Some(VarRegister(name))
      case Asts.MeRef() =>
        Some(MeRegister)
      case Asts.PackageRef(pkgName) =>
        Some(PackageRegister(pkgName))
      case Asts.DeviceRef(device) =>
        Some(DeviceRegister(device))
      case call@Asts.Call(receiverOpt, function, args) =>
        val (receiverType, funSig) = call.getSignature.get
        val argsRegisters = args.map(compile(_).get)
        val isVoid = funSig.retType == VoidType
        val receiverRegisterOpt = if isVoid then None else Some(new GenRegister)
        addInstr(Invocation(receiverType, funSig, argsRegisters, receiverRegisterOpt, call.getPosition))
        receiverRegisterOpt
      case Asts.Indexing(indexed, arg) =>
        val indexedRegister = compile(indexed).get
        ???   // TODO unboxing
      case Asts.ArrayInit(region, elemType, size) => ???
      case Asts.FilledArrayInit(regionOpt, arrayElems) => ???
      case Asts.StructOrModuleInstantiation(regionOpt, typeId, args) => ???
      case Asts.UnaryOp(operator, operand) => ???
      case Asts.BinaryOp(lhs, operator, rhs) => ???
      case Asts.Select(lhs, selected) => ???
      case Asts.Ternary(cond, thenBr, elseBr) => ???
      case Asts.Cast(expr, tpe) => ???
      case Asts.TypeTest(expr, tpe) => ???
      case Asts.Sequence(stats, expr) => ???
      case Asts.Block(stats) => ???
      case Asts.LocalDef(localName, optType, rhs, isReassignable) => ???
      case Asts.VarAssig(lhs, rhs) => ???
      case Asts.VarModif(lhs, rhs, op) => ???
      case Asts.IfThenElse(cond, thenBr, elseBrOpt) => ???
      case Asts.WhileLoop(cond, body) => ???
      case Asts.ForLoop(initStats, cond, stepStats, body) => ???
      case Asts.ReturnStat(optVal) => ???
      case Asts.PanicStat(msg) => ???
      case Asts.EnclosedStat(capabilities, body) => ???
      case Source(defs) => ???
      case Asts.PackageDef(packageName, functions) => ???
      case Asts.ModuleDef(moduleName, imports, functions) => ???
      case Asts.StructDef(structName, fields, directSupertypes, isInterface) => ???
      case Asts.ConstDef(constName, tpeOpt, value) => ???
      case Asts.ParamImport(paramId, paramType) => ???
      case Asts.PackageImport(packageId) => ???
      case Asts.DeviceImport(device) => ???
      case Asts.FunDef(funName, params, optRetType, body) => ???
      case Param(paramNameOpt, tpe, isReassignable) => ???
    }
    
    CaptureFlow(basicBlocks.toList)
  }

  private def computeCaptures(captureFlow: CaptureFlow, params: List[Param], expectedRetCd: CaptureDescriptor): Unit = {
    given capturesTracker: CapturesTracker = CapturesTracker()

    given fieldsTracker: FieldsTracker = FieldsTracker()

    val execStartState = State.fromFunParams(params)
    val startStates = mutable.Map.empty[BasicBlock, State]
    val bbsToExplore = mutable.Queue.empty[BasicBlock]

    def saveDest(bb: BasicBlock, state: State): Unit = {
      if (!startStates.get(bb).contains(state)) {
        bbsToExplore.enqueue(bb)
      }
    }

    saveDest(captureFlow.startBB, execStartState)
    while (bbsToExplore.nonEmpty) {
      val currBB = bbsToExplore.dequeue()
      val bbStartState = startStates.apply(currBB)

      given workState: State = bbStartState.deepCopy()

      executeBB(currBB) match {
        case Conditional(cond, thenBr, elseBr, position) =>
          saveDest(thenBr, workState)
          saveDest(elseBr, workState)
        case Goto(target, position) =>
          saveDest(target, workState)
        case Return(result, position) =>
          eval(result)
        case End => ()
      }
    }
  }

  private def executeBB(block: BasicBlock)(using state: State, ft: FieldsTracker, ct: CapturesTracker): BBTerminator = {

    @tailrec def execute(instructions: List[BBInstr]): BBTerminator = instructions match {
      case Assignment(dst, src, position) :: rem =>
        state(dst) = eval(src)
        execute(rem)
      case Eval(expr, position) :: rem =>
        eval(expr)
        execute(rem)
      case (terminator: BBTerminator) :: _ => terminator
      case Nil => throw new AssertionError()
    }

    execute(block.instructions)
  }

  private def eval(expr: Expr)(using state: State, ft: FieldsTracker, ct: CapturesTracker): CapturableValue = expr match
    case NewValue(value) => ???
    case Read(varId) => ???
    case SubcapturingConstraint(exp, expr) => ???
}
