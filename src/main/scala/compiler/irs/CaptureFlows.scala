package compiler.irs

import compiler.capturechecker.{CaptureDescriptor, CaptureSet, Register}
import compiler.reporting.Position
import identifiers.TypeIdentifier
import lang.{FunctionSignature, CapturableValue}
import lang.Types.NamedType

object CaptureFlows {

  final case class CaptureFlow(blocks: List[BasicBlock]) {
    def startBB: BasicBlock = blocks.head
  }

  final case class BasicBlock(instructions: List[BBInstr]) {
    def startInstr: BBInstr = instructions.head
  }

  sealed trait BBInstr {
    def position: Option[Position]
  }
  final case class Assignment(dst: Register, src: Expr, override val position: Option[Position]) extends BBInstr
  final case class Invocation(
                               receiverType: NamedType,
                               funSig: FunctionSignature,
                               args: List[Register],
                               resultRegisterOpt: Option[Register],
                               override val position: Option[Position]
                             ) extends BBInstr
  final case class Eval(expr: Expr, override val position: Option[Position]) extends BBInstr

  sealed trait BBTerminator extends BBInstr
  final case class Conditional(cond: Expr, thenBr: BasicBlock, elseBr: BasicBlock, override val position: Option[Position]) extends BBTerminator
  final case class Goto(target: BasicBlock, override val position: Option[Position]) extends BBTerminator
  final case class Return(result: Expr, override val position: Option[Position]) extends BBTerminator
  final case class End(override val position: Option[Position]) extends BBTerminator


  sealed abstract class Expr {
    private var _captureDescr: CaptureDescriptor = CaptureSet.empty

    def saveMayCapture(cd: CaptureDescriptor): Unit = {
      _captureDescr = _captureDescr.union(cd)
    }

    def captureDescr: CaptureDescriptor = _captureDescr
  }

  final case class NewValue(value: CapturableValue) extends Expr
  final case class Read(register: Register) extends Expr

  final case class SubcapturingConstraint(exp: CaptureDescriptor, expr: Expr) extends Expr

}
