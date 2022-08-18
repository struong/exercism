object FlattenArray {
  def flatten(value: List[_]): List[_] = value
    .filterNot(_ == null)
    .flatMap {
      case l: List[_] => flatten(l)
      case elem       => List(elem)
    }
}
