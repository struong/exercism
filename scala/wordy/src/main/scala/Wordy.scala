import cats.parse.Numbers.signedIntString
import cats.parse.Parser
// a collection of useful parsers
import cats.parse.Rfc5234._
import cats.parse.Parser.{string => pstring, char => pchar}
import scala.language.postfixOps

object Wordy {
  final case class Op(l: Int, operator: String, r: Int)

  def answer(input: String): Option[Int] = {

    val digitsParser = signedIntString.map(_.toInt)
    val opParser = (Parser.string("plus") | Parser.string("minus")).surroundedBy(sp).string
    val whatIsParser = pstring("What is") ~ sp
    val parser: Parser[Op] = (digitsParser ~ opParser ~ digitsParser).map(Op.tupled)

    println(s"parser.parse(input) = ${ parser.parse(input)}")
    
    val result = parser.parse(input).map {
      case (rest, plus) =>
        println(s"rest = ${rest}")
        plus.r + plus.l
    }

    result.toOption
  }
}
