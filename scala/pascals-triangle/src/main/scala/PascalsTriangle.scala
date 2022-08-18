object PascalsTriangle {

  def newRow(lastRow: List[Int]): List[Int] = {
    val pascal = for {
      (v, index) <- lastRow.zipWithIndex
    } yield {
      val previous = lastRow.lift(index - 1).getOrElse(0)
      v + previous
    }

    pascal :+ 1
  }

  def makeRows(n: Int, triangle: List[List[Int]]): List[List[Int]] = {
    if (n == triangle.length) triangle
    else makeRows(n, triangle :+ newRow(triangle.last))
  }

  def rows(n: Int): List[List[Int]] =
    n match {
      case n if (n <= 0) => List.empty
      case 1 => List(List(1))
      case n => makeRows(n, List(List(1), List(1, 1)))
    }
}
