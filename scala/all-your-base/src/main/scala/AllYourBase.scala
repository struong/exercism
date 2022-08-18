import math._
import scala.annotation.tailrec

object AllYourBase {
  def toDecimal(inBase: Int, values: List[Int]): Int =
    values.reverse.zipWithIndex
      .map { case (value, count) =>
        value * pow(inBase, count)
      }
      .map(_.toInt)
      .sum

  @tailrec
  def convert(outBase: Int, value: Int, result: List[Int]): List[Int] =
    if (value < outBase) value :: result
    else convert(outBase, value / outBase, value % outBase :: result)

  def isInvalidInput(inBase: Int, values: List[Int], outBase: Int): Boolean =
    inBase < 2 || outBase < 2 || values.exists(_ <= -1) || values.exists(
      _ >= inBase
    )

  def rebase(
      inBase: Int,
      values: List[Int],
      outBase: Int
  ): Option[List[Int]] = {
    if (isInvalidInput(inBase, values, outBase)) None
    else
      Some(convert(outBase, toDecimal(inBase, values), List.empty))
  }
}
