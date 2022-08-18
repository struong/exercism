object Hamming {
  def distance(left: String, right: String): Option[Int] = {
    if (left.size != right.size)
      None
    else {
      Some(
        (left zip right).count {
          case (l, r) => l != r
        }
      )
    }
  }
}