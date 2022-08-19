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
    var mines = 0
    val range = -1 to 1

    for(i <- range) { 
      for(j <- range) { 
          if(i != 0 || j != 0) { 
            if(hasMine(x + i, y + j, input)) { 
               mines = mines + 1
            }
          }
      }
    }

    mines
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
