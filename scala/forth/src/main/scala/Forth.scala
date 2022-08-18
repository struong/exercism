import ForthError._

trait ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

case class Program(
    userWords: List[String],
    execWords: String,
    definitions: Map[String, Definition]
) {
  def replace: Program = {
    userWords.foldRight(this) { case (userWord, program) =>
      userWord.split(" ").toList match {
        case x :: xs =>
          val replacement = xs.mkString(" ")
          if (definitions.contains(x) && xs.size == 1) {
            val newDefinitions = definitions + (x -> definitions(replacement))
            copy(definitions = newDefinitions)
          } else
            copy(execWords = program.execWords.replaceAll(x, replacement))

        case _ => program
      }
    }
  }
}

object Program {
  val definitions: Map[String, Definition] = Map(
    "+" -> AddDefinition,
    "-" -> SubtractDefinition,
    "*" -> MultiplyDefinition,
    "/" -> DivideDefinition,
    "dup" -> DuplicateDefinition,
    "drop" -> DropDefinition,
    "swap" -> SwapDefinition,
    "over" -> OverDefinition
  )

  def apply(text: String): Either[ForthError, Program] = {
    if (text.contains(":")) {
      val words = text.replaceAll(":", " ").split(";").map(_.trim).toList
      words match {
        case xs :+ end =>
          if (xs.isEmpty || end.isEmpty)
            Left(InvalidWord)
          else
            Right(Program(xs, end, definitions))
        case _ => Right(Program(List.empty, text, definitions))
      }
    } else {
      Right(Program(List.empty, text, definitions))
    }
  }
}

class Forth extends ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    val caseInsensitiveText = text.toLowerCase

    for {
      programE <- Program(caseInsensitiveText)
      program = programE.replace
      result <- program.execWords
        .split(" ")
        .toList
        .foldLeft(
          Right(ForthEvaluatorState(List.empty)): Either[
            ForthError,
            ForthEvaluatorState
          ]
        ) { case (state, op) =>
          op.toIntOption match {
            case Some(literal) => state.map(x => x.copy(x.stack :+ literal))
            case None =>
              op match {
                case "+" | "-" | "*" | "/" | "dup" | "drop" | "swap" | "over" =>
                  program
                    .definitions(op)
                    .evaluate(state)
                case _ => Left(UnknownWord)
              }
          }
        }
    } yield {
      result
    }
  }
}
