case class School(data: Map[Int, Seq[String]] = Map.empty[Int, Seq[String]]) {
  type DB = Map[Int, Seq[String]]

  def add(name: String, g: Int): School = copy(data.updatedWith(g) {
    case Some(value) => Some(value :+ name)
    case None => Some(Seq(name))
  })

  def db: DB = data

  def grade(g: Int): Seq[String] = data.getOrElse(g, Seq.empty[String])

  def sorted: DB = data.view.mapValues(_.sorted).toSeq.sortBy(_._1).toMap
}

