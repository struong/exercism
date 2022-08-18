import ForthError.{DivisionByZero, ForthError, InvalidWord, StackUnderflow}

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

case class ForthEvaluatorState(stack: List[Int]) {
  override def toString: String = stack.mkString(" ")
}

abstract class Definition {
  def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState]
}

object OverDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 1, StackUnderflow)
      .map { x =>
        val overStack = x.stack match {
          case xs :+ end1 :+ end2 => xs :+ end1 :+ end2 :+ end1
          case _                  => throw new RuntimeException("impossible to get here")
        }
        x.copy(stack = overStack)
      }
}

object SwapDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 1, StackUnderflow)
      .map { x =>
        val swappedStack = x.stack match {
          case xs :+ end1 :+ end2 => xs :+ end2 :+ end1
          case _                  => throw new RuntimeException("impossible to get here")
        }
        x.copy(stack = swappedStack)
      }
}

object DropDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 0, StackUnderflow)
      .map(x => x.copy(stack = x.stack.dropRight(1)))
}

object DuplicateDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 0, StackUnderflow)
      .map(x => x.copy(stack = x.stack :+ x.stack.last))
}

object AddDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 1, InvalidWord)
      .map(x => x.copy(stack = List(x.stack.sum)))
}

object SubtractDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 1, InvalidWord)
      .map(x => x.copy(stack = List(x.stack.reduceLeft(_ - _))))
}

object MultiplyDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(_.stack.length > 1, InvalidWord)
      .map(x => x.copy(stack = List(x.stack.product)))
}

object DivideDefinition extends Definition {
  override def evaluate(
      state: Either[ForthError, ForthEvaluatorState]
  ): Either[ForthError, ForthEvaluatorState] =
    state
      .filterOrElse(!_.stack.contains(0), DivisionByZero)
      .filterOrElse(_.stack.length > 1, InvalidWord)
      .map(x => x.copy(stack = List(x.stack.reduceLeft(_ / _))))
}
