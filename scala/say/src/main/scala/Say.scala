object Say {
  val ones = Seq(
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )
  val teens = Seq(
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
  )
  val tens = Seq(
    "ten",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
  )
  val hundred = "hundred"
  val thousands =
    Seq("thousand", "million", "billion")

  def inEnglish(n: Long): Option[String] = {
    n match {
      case _ if n < 0 || n > 999999999999L => None
      case _                               => Some(say(n))
    }
  }

  def say(n: Long): String = n match {
    case _ if n < 10 => ones(n.toInt)
    case _ if n < 20 => teens(n.toInt - 10)
    case _ if n < 100 =>
      val remainder = n % 10
      val index = (n.toInt / 10) - 1
      if (remainder == 0)
        tens(index)
      else {
        tens(index) + "-" + ones(n.toInt % 10)
      }
    case _ if n < 1000 =>
      val remainder = n % 100
      val index = (n.toInt / 100)
      if (remainder == 0)
        ones(index) + " " + hundred
      else
        ones(index) + " " + hundred + " " + say(remainder)
    case _ if n < 1000000 =>
      val remainder = n % 1000
      if (remainder == 0)
        say(n / 1000) + " " + thousands(0)
      else
        say(n / 1000) + " " + thousands(0) + " " + say(remainder)
    case _ if n < 1000000000 =>
      val remainder = n % 1000000
      if (remainder == 0)
        say(n / 1000000) + " " + thousands(1)
      else
        say(n / 1000000) + " " + thousands(1) + " " + say(remainder)
    case _ =>
      val remainder = n % 1000000000
      if (remainder == 0)
        say(n / 1000000000) + " " + thousands(2)
      else
        say(n / 1000000000) + " " + thousands(2) + " " + say(remainder)
  }
}
