object Minesweeper {
  def annotate(input: List[String]): List[String] = {
    input match {
      case Nil => List.empty
      case _   => stuff(input.map(_.toList))
    }
  }

  def stuff(input: List[List[Char]]): List[String] = {
    println(input)

    var board = Array.fill(input.size, input.size)(' ')

    for (row <- 0 until input.size) {
      for (column <- 0 until input(row).size) {
        if (input.lift(row).flatMap(_.lift(column)).contains('*')) {
          board(row)(column) = '*'
        } else {
          val topLeftHasMine =
            input.lift(row - 1).flatMap(_.lift(column - 1)).contains('*')
          val topHasMine =
            input.lift(row - 1).flatMap(_.lift(column)).contains('*')
          val topRightHasMine =
            input.lift(row - 1).flatMap(_.lift(column + 1)).contains('*')

          val leftHasMine =
            input.lift(row).flatMap(_.lift(column - 1)).contains('*')
          val rightHasMine =
            input.lift(row).flatMap(_.lift(column + 1)).contains('*')

          val bottomLeftHasMine =
            input.lift(row + 1).flatMap(_.lift(column - 1)).contains('*')
          val bottomHasMine =
            input.lift(row + 1).flatMap(_.lift(column)).contains('*')
          val bottomRightHasMine =
            input.lift(row + 1).flatMap(_.lift(column + 1)).contains('*')

          println(
            s"minesSeq = ${Seq(topLeftHasMine, topHasMine, topRightHasMine, leftHasMine, rightHasMine, bottomLeftHasMine, bottomHasMine, bottomRightHasMine)}"
          )
          val mines = Seq(
            topLeftHasMine,
            topHasMine,
            topRightHasMine,
            leftHasMine,
            rightHasMine,
            bottomLeftHasMine,
            bottomHasMine,
            bottomRightHasMine
          ).count(_ == true)
          println(s"row = $row")
          println(s"column = $column")
          println(s"mines = $mines")

          if(mines != 0) 
            board(row)(column) = mines.toString().charAt(0)
        }
      }
    }

    board.toList.map(_.mkString)
  }
}
