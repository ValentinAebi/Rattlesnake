package compiler.parser

import compiler.Position
import compiler.irs.Tokens.*

/**
 * Iterator allowing at most one lookahead
 */
final class LL1Iterator private(tokensSeq: Iterable[PositionedToken]) {
  require(tokensSeq.nonEmpty)
  private val _iterator: Iterator[PositionedToken] = tokensSeq.iterator
  private var _current: Option[PositionedToken] = _iterator.nextOption()
  private var _previous: Option[PositionedToken] = None
  private var _ignoreEndl: Boolean = true

  /**
   * Lookahead method
   * @return the token the iterator is currently pointing to (unless none remain)
   */
  def currentOpt: Option[PositionedToken] = _current

  /**
   * Moves the iterator 1 token forward (unless it is empty)
   * @return the token the iterator was pointing to before the call to `movesForwardOpt`
   */
  def moveForwardOpt(): Option[PositionedToken] = {
    _previous = _current

    def next(): Unit = {
      _current = _iterator.nextOption()
      if (_ignoreEndl && _current.isDefined && _current.get.token == EndlToken) next()
    }

    next()
    _previous
  }

  /**
   * @return the position the iterator stopped at, that is,
   *         the start position of the current token if there is one such token,
   *         or the end position of the last token if no token remain
   *
   * <b>WARNING</b> will crash if the iterator was moved more than once after becoming empty
   */
  def lastPosition: Position = _current.map(_.position).getOrElse {
    val prev = _previous.get
    val pos = prev.position
    pos.copy(col = pos.col + prev.token.strValue.length)
  }
  
  def lastPositionOpt: Option[Position] = _current.map(_.position)

  /**
   * @return a list with all remaining tokens
   */
  def remainingAsList: List[PositionedToken] = (_current ++ _iterator.toList).toList

  /**
   * Configures whether this iterator should skip `EndlToken`s
   */
  def setIgnoreEndl(ignoreEndl: Boolean): Unit = {
    _ignoreEndl = ignoreEndl
    while (currentOpt.isDefined && currentOpt.get.token == EndlToken){
      _current = _iterator.nextOption()
    }
  }
  
}

object LL1Iterator {
  def from(tokensSeq: Iterable[PositionedToken]): LL1Iterator = {
    new LL1Iterator(tokensSeq)
  }
}
