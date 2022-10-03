import cats.data.{NonEmptyList => Nel}
import cats.parse.Numbers.signedIntString
import cats.parse.Parser
import cats.parse.Parser.{string => pstring}
import cats.parse.Rfc5234._

import scala.language.postfixOps

object Wordy {
  final case class Op(firstDigit: Int, operations: Nel[(String, Int)])

  def answer(input: String): Option[Int] = {

    val whatIsParser = (pstring("What is") ~ sp).void
    val digitsParser = signedIntString.map(_.toInt)

    val opParser =
      (Parser.string("plus") | Parser.string("minus") | Parser.string(
        "divided by"
      ) | Parser.string(
        "multiplied by"
      )).surroundedBy(sp).string ~ digitsParser

    val parser =
      ((whatIsParser *> digitsParser) ~ opParser.rep).map(Op.tupled)

    val result = parser.parse(input).map { case (_, op) =>
      op.operations.foldLeft(op.firstDigit) { case (accum, (operator, value)) =>
        operator.trim match {
          case "plus"          => accum + value
          case "minus"         => accum - value
          case "multiplied by" => accum * value
          case "divided by"    => accum / value
        }
      }
    }

    result.toOption
  }
}
