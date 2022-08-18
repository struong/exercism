import cats.Show
import cats.syntax.show._

sealed trait Responses
case object Whatever extends Responses
case object Shouting extends Responses
case object Question extends Responses
case object ForcefulQuestion extends Responses
case object Silence extends Responses

object Bob {
  implicit val ResponsesShow: Show[Responses] = new Show[Responses] {
    override def show(responses: Responses): String = responses match {
      case Whatever => "Whatever."
      case Shouting => "Whoa, chill out!"
      case Question => "Sure."
      case ForcefulQuestion => "Calm down, I know what I'm doing!"
      case Silence => "Fine. Be that way!"
    }
  }

  implicit class BobsResponse(statement: String) {
    def responses: Responses = {

      val letters = statement.filter(_.isLetter)

      statement.trim match {
        case s if s.isEmpty => Silence
        case s if letters.isEmpty && s.endsWith("?") => Question
        case s if letters.isEmpty => Whatever
        case s if letters.toUpperCase == letters && s.endsWith("?") => ForcefulQuestion
        case _ if letters.toUpperCase == letters => Shouting
        case s if s.endsWith("?") => Question
        case _ => Whatever
      }
    }
  }

  def response(statement: String): String = statement.responses.show
}
