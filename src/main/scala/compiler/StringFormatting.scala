package compiler

object StringFormatting {

  def stringLengthLimited(maxLen: Int, str: String): String = {
    if str.length > maxLen then s"${str.length}..."
    else str
  }

}
