object Minesweeper {
  def annotate(input: List[String]): List[String] = {
    input match {
      case Nil => List.empty
      case _   => mapMines(input.map(_.toList))
    }
  }

  def hasMine(x: Int, y: Int, input: List[List[Char]]): Boolean =
    input.lift(x).flatMap(_.lift(y)).contains('*')

  def neighbourMines(x: Int, y: Int, input: List[List[Char]]): Int = {
    val range = (-1 to 1)

    range.foldLeft(0) { case (totalMines, i) =>
      val currentMineCount = range.foldLeft(0) { case (minesAcc, j) =>
        if ((i != 0 || j != 0) && (hasMine(x + i, y + j, input))) {
          minesAcc + 1
        } else
          minesAcc
      }
      
      totalMines + currentMineCount
    }
  }

  def mapMines(input: List[List[Char]]): List[String] = {
    val board = Array.fill(input.size, input.headOption.fold(0)(_.size))(' ')

    for (row <- 0 until input.size) {
      for (column <- 0 until input(row).size) {
        if (hasMine(row, column, input)) {
          board(row)(column) = '*'
        } else {
          val mines = neighbourMines(row, column, input)
          if (mines != 0)
            board(row)(column) = (48 + mines).toChar
        }
      }
    }

    board.toList.map(_.mkString)
  }
}
