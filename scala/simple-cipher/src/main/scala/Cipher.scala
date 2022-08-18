case class Cipher(keyOpt: Option[String]) {


  val key: String = keyOpt.fold("aaaaaaaaaa") { str =>
    if(str.toLowerCase != str) throw new IllegalArgumentException
    else if(str forall Character.isDigit) throw new IllegalArgumentException
    else if(str.isEmpty) throw new IllegalArgumentException
    else str
  }

  def encode(plainText: String): String = {
    plainText.zip(key).map {
      case (p, k) => (((p + k - 2 * 'a') % 26) + 'a').toChar
    }.mkString
  }

  def decode(encoded: String): String = {
    encoded.zip(key).map {
      case (e, k) => (((e - k) % 26) + 'a').toChar
    }.mkString
  }
}

