import scala.annotation.tailrec

object CollatzConjecture {

  def steps(n: Int): Option[Int] = {

    @tailrec
    def go(n: Int, steps: Int): Option[Int] = {
      n match {
        case i if i <= 0  => None
        case i if i == 1 => Some(steps)
        case i =>
          if (i % 2 == 0) go(i / 2, steps + 1) else go((3 * i) + 1, steps + 1)
      }
    }

    go(n, 0)
  }
}
