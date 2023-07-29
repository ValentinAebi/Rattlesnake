package identifiers

sealed trait Identifier {

  def stringId: String

  override def toString: String = stringId
}

sealed trait FunOrVarId extends Identifier

sealed trait TypeIdentifier extends Identifier

sealed trait StructIdentifier extends TypeIdentifier

final case class NormalFunOrVarId(stringId: String) extends FunOrVarId
final case class NormalStructId(stringId: String) extends StructIdentifier

case object StringEqualityFunId extends FunOrVarId {
  val rawName = "stringEq"

  override val stringId: String = "streq$" ++ rawName
}

final case class DesugarerGeneratedVarId(private val rawName: String) extends FunOrVarId {
  override def stringId: String = "des$" ++ rawName
}

final case class BackendGeneratedVarId(private val rawName: String) extends FunOrVarId {
  override def stringId: String = "bck$" ++ rawName
}

object BackendGeneratedVarId {
  def apply(varIdx: Int): BackendGeneratedVarId = new BackendGeneratedVarId(varIdx.toString)
}
