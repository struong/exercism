import cats.Show
import cats.syntax.show._

sealed trait Person
case class Someone(name: String) extends Person
case object NoOne extends Person

object Twofer {
  implicit val twoferShow: Show[Person] = new Show[Person] {
    override def show(person: Person): String = {
      val name = person match {
        case Someone(name) => name
        case NoOne => "you"
      }

      s"One for $name, one for me."
    }
  }

  implicit class TwoFerPerson(name: String) {
    def toPerson: Person = if(name.isEmpty) NoOne else Someone(name)
  }

  def twofer(name: String = ""): String = name.toPerson.show
}
