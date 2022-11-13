package compiler.lexer

import compiler.lexer.Matchers.*
import compiler.Errors.{CompilationError, Err, ErrorReporter, Fatal}
import compiler.{CompilerStep, SourceCodeProvider}
import compiler.irs.Tokens.*
import lang.Keyword
import lang.Operator
import compiler.CompilationStep
import compiler.StringFormatting.stringLengthLimited
import compiler.Position

import scala.util.{Failure, Success}
import lang.Types.PrimitiveType

import scala.annotation.tailrec

final class Lexer(errorReporter: ErrorReporter) extends CompilerStep[SourceCodeProvider, (List[PositionedToken], String)] {

  private val uppercaseLetter: Matcher = charInRange('A', 'Z')
  private val lowercaseLetter: Matcher = charInRange('a', 'z')

  private val digit: Matcher = charInRange('0', '9')
  private val digitsSeq: Matcher = stringInRange('0', '9')

  private val underscore: Matcher = char('_')
  private val letterDigitUnderscore: Matcher = uppercaseLetter | lowercaseLetter | digit | underscore

  private val keywordsMatchers: List[String => Option[(Token, String, Int)]] =
    Keyword.values.toList.map { kw =>
      string(kw.str) into KeywordToken(kw)
    }

  private val operatorsMatchers: List[String => Option[(Token, String, Int)]] =
    Operator.values.toList.map { op =>
      string(op.str) into OperatorToken(op)
    }

  private val explicitelyDefinedMatchersByPriorityOrder: List[String => Option[(Token, String, Int)]] = List(
    string(_.isWhitespace) into SpaceToken,
    string("true") | string("false") into { str => BoolLitToken(str.toBoolean) },
    uppercaseLetter ++ repeat(letterDigitUnderscore) into { FirstUppercaseIdentifierToken(_) },
    lowercaseLetter ++ repeat(letterDigitUnderscore) into { FirstLowercaseIdentifierToken(_) },
    digitsSeq intoOpt { str => str.toIntOption.map(IntLitToken.apply) },
    digitsSeq ++ char('.') ++ digitsSeq intoOpt { str => str.toDoubleOption.map(DoubleLitToken.apply) },
    char('\'') ++ anyChar ++ char('\'') into { str => CharLitToken(str(1)) },
    char('"') ++ repeat(anyCharBut('"')) ++ char('"') into { str => StringLitToken(str.drop(1).dropRight(1)) },
    string("//") ++ repeat(anyChar) into { CommentToken(_) },
  )

  private val matchersByPriorityOrder =
    keywordsMatchers ++ operatorsMatchers ++ explicitelyDefinedMatchersByPriorityOrder

  private def tokenizeLine(line: String, lineIdx: Int, srcCodeProvider: SourceCodeProvider): List[(Token, Int)] = {

    // takes longest match, if two candidates with maximum match then first in order is taken
    @tailrec def tokenizeRemaining(rem: String, tokensReversed: List[(Token, Int)], col: Int): List[(Token, Int)] = {
      if rem.isEmpty then ((EndlToken, col) :: tokensReversed).reverse
      else {
        val matcherRes = matchersByPriorityOrder
          .flatMap(_.apply(rem))
          .maxByOption(_._3)
        matcherRes match {
          case Some((newTok, newRem, len)) => tokenizeRemaining(newRem, (newTok, col) :: tokensReversed, col + len)
          case None => {
            errorReporter.push(Err(CompilationStep.Lexing,
              s"syntax error: '${stringLengthLimited(20, rem)}'", Some(Position(srcCodeProvider, lineIdx, col))))
            val splitIdx = rem.indexOf(' ')
            if (splitIdx >= 0) {
              val (errTokStr, newRem) = rem.splitAt(splitIdx)
              tokenizeRemaining(newRem, (ErrorToken(errTokStr), col) :: tokensReversed, col + splitIdx)
            }
            else ((ErrorToken(rem), col) :: tokensReversed).reverse
          }
        }
      }
    }

    tokenizeRemaining(line, Nil, 0)
  }

  override def apply(sourceCodeProvider: SourceCodeProvider): (List[PositionedToken], String) = {
    sourceCodeProvider.lines match {
      case Failure(_) => errorReporter.pushFatal(Fatal(CompilationStep.Lexing,
        s"could not read source code from source '${sourceCodeProvider.name}'", None))
      case Success(lines) => {
        val tokenizedLines = lines.toList.zipWithIndex.map((line, idx) => tokenizeLine(line, idx, sourceCodeProvider))
        val positionedTokens =
          for (line, lineIdx) <- tokenizedLines.zipWithIndex; (token, colIdx) <- line yield {
            PositionedToken(token, Position(sourceCodeProvider, line = lineIdx + 1, col = colIdx + 1))
          }
        (positionedTokens, sourceCodeProvider.name)
      }
    }
  }

}
