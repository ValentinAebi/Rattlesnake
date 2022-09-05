package compiler

trait CompilerStep[In, Out] {
  thisStep =>

  def apply(input: In): Out

  final def andThen[NextIn >: Out, NextOut](nextStep: CompilerStep[NextIn, NextOut]): CompilerStep[In, NextOut] = {
    new CompilerStep[In, NextOut] {
      override def apply(input: In): NextOut = {
        val func = thisStep.apply.andThen(nextStep.apply)
        func.apply(input)
      }
    }
  }

}
