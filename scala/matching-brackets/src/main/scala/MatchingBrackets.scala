import cats._
import cats.implicits._
import cats.data.State

sealed trait Status

case object Closed extends Status

case object Open extends Status

case object Invalid extends Status

object MatchingBrackets {
  val opposite = Map(
    "(" -> ")",
    "[" -> "]",
    "{" -> "}",
    "<" -> ">"
  )

  type MatchingState[A] = State[List[String], A]

  def evalOne(symbol: String): MatchingState[Status] = {
    symbol match {
      case x@("(" | "[" | "{" | "<") => opening(x)
      case x@(")" | "]" | "}" | ">") => closing(x)
      case _ => Closed.pure[MatchingState]
    }
  }

  def opening(symbol: String): MatchingState[Status] = {
    State[List[String], Status] { stack =>
      (symbol :: stack, Open)
    }
  }

  def closing(symbol: String): MatchingState[Status] = {
    State[List[String], Status] {
      case head :: tail =>
        val status = if (opposite.get(head).contains(symbol)) {
          Closed
        } else {
          Invalid
        }

        (tail, status)
      case list => (list, Invalid)
    }
  }

  def evalAll(input: List[String]): MatchingState[Status] = {
    input.foldLeft(Closed.pure[MatchingState]) { (a, b) =>
      a.flatMap { _ => evalOne(b) }
    }
  }

  def isPaired(str: String): Boolean = {
    val program = evalAll(str.toList.map(_.toString))
    program.runA(Nil).value == Closed
  }

}