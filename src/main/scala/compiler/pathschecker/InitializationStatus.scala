package compiler.pathschecker

enum InitializationStatus {
  case Initialized, Uninitialized, PossiblyUninitialized
  
  def joined(that: InitializationStatus): InitializationStatus = (this, that) match {
    case (s1, s2) if s1 == s2 => s1
    case _ => PossiblyUninitialized
  }

}
