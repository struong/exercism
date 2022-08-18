import scala.annotation.tailrec

trait DominoesAlgebra[F[_]] {
  type Domino = List[(Int, Int)]
  def chain(domino: Domino): F[Domino]
}

object DominoesAlgebra {

  def apply[F[_]](implicit dt: DominoesAlgebra[F]): DominoesAlgebra[F] = dt

  implicit object DominoesInterpreter extends DominoesAlgebra[Option] {
    def startAndEndMatches(domino: Domino): Boolean =
      domino.head._1 == domino.last._2

    @tailrec
    def validChain(domino: Domino, accum: Option[Domino]): Option[Domino] = {
      domino match {
        case Nil         => Some(Nil)
        case last :: Nil => accum.map(last :: _)
        case x :: y :: ys if (x._2 == y._1) =>
          validChain(y :: ys, accum.map(x :: _))
        case x :: y :: ys if (x._2 == y._2) =>
          validChain(y.swap :: ys, accum.map(x :: _))
        case _ => None
      }
    }

    override def chain(domino: Domino): Option[Domino] = {
      if (domino.isEmpty) Some(List.empty)
      else {
        domino.permutations
          .flatMap(_.sliding((domino.size)))
          .filter(startAndEndMatches)
          .flatMap(x => validChain(x, Some(List.empty[(Int, Int)])))
          .find(_.nonEmpty)
          .map(_.reverse)
      }
    }
  }
}

object Dominoes {
  def chain[F[_]](domino: List[(Int, Int)])(implicit D: DominoesAlgebra[F]): F[List[(Int, Int)]] = {
    val result =  D.chain(domino)
    println(s"result = $result")
    result
  }
}
