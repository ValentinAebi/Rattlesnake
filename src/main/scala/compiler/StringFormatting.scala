package compiler

object StringFormatting {

  /**
   * @return `str`, unless it is too long in which case it is truncated and postfixed with "..."
   */
  def stringLengthLimited(maxLen: Int, str: String): String = {
    if str.length > maxLen then s"$str..."
    else str
  }

}
