object Anagram {
  def convertToMap(str: String): Map[Char, Int] = {
    str.foldLeft(Map.empty[Char, Int]) {
      case (map: Map[Char, Int], c: Char) =>
        map.get(c) match {
          case Some(value) => map.updated(c, value + 1)
          case _ => map + (c -> 1)
        }
    }
  }

  def matcher(str1: String, str2: String): Boolean = {
    if (str1.toLowerCase() == str2.toLowerCase())
      false
    else {
      val map1 = convertToMap(str1.toLowerCase())
      val map2 = convertToMap(str2.toLowerCase())

      map1 == map2
    }
  }

  def findAnagrams(word: String, input: List[String]): List[String] = {
    input.filter(i => matcher(word, i))
  }
}