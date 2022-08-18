import scala.annotation.tailrec
sealed trait Direction {
  def next: Direction
}
case object Up extends Direction {
  override def next: Direction = Right
}
case object Down extends Direction {
  override def next: Direction = Left
}
case object Right extends Direction {
  override def next: Direction = Down
}
case object Left extends Direction {
  override def next: Direction = Up
}

object SpiralMatrix {
  def spiralMatrix(n: Int): List[List[Int]] =
    n match {
      case 0 => List.empty
      case _ =>
        buildMatrix(0, 0, Right, Array.ofDim(n, n)).map(_.toList).toList
    }

  @tailrec
  def buildMatrix(
      row: Int,
      column: Int,
      direction: Direction,
      matrix: Array[Array[Int]]
  ): Array[Array[Int]] = {
    val count =  matrix.flatten.count(_ != 0)
    if (count == matrix.size * matrix.size) matrix
    else {
      if (
        matrix.lift(row).flatMap(_.lift(column)).isDefined && matrix(column)(
          row
        ) == 0
      ) {
        matrix(column)(row) = count + 1
        direction match {
          case Up =>
            buildMatrix(row, column - 1, direction, matrix)
          case Down =>
            buildMatrix(row, column + 1, direction, matrix)
          case Right =>
            buildMatrix(row + 1, column, direction, matrix)
          case Left =>
            buildMatrix(row - 1, column, direction, matrix)
        }
      } else {
        direction match {
          case Up =>
            buildMatrix(row + 1, column + 1, direction.next, matrix)
          case Down =>
            buildMatrix(row - 1, column - 1, direction.next, matrix)
          case Right =>
            buildMatrix(row - 1, column + 1, direction.next, matrix)
          case Left =>
            buildMatrix(row + 1, column - 1, direction.next, matrix)
        }
      }
    }
  }
}
