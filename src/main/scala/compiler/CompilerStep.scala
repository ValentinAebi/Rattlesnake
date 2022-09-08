package compiler

trait CompilerStep[In, Out] {
  thisStep =>

  def apply(input: In): Out

  final def andThen[NextIn >: Out, NextOut](nextStep: CompilerStep[NextIn, NextOut]): CompilerStep[In, NextOut] = {
    (input: In) => {
      val func = thisStep.apply.andThen(nextStep.apply)
      func.apply(input)
    }
  }

}

final case class MultiStep[In, Out](threadPipeline: CompilerStep[In, Out]) extends CompilerStep[List[In], List[Out]] {

  override def apply(input: List[In]): List[Out] = {
    input.map(threadPipeline.apply)
  }
}

final case class Mapper[In, Out](f: In => Out) extends CompilerStep[In, Out] {
  override def apply(input: In): Out = f(input)
}
