object RomanNumerals {

  def roman(i: Int): String = {
    val thousands: String = "M" * (i / 1000)


    val hundreds: String = {
      val values = Seq("C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
      if (i % 1000 > 99) {
        val accessor = i % 1000 / 100
        values(accessor - 1)
      } else ""
    }

    val tens: String = {
      val values = Seq("X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
      if (i % 100 > 9) {
        val accessor = i % 100 / 10
        values(accessor - 1)
      } else ""
    }

    val ones: String = {
      val values = Seq("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
      if (i % 10 > 0) {
        val accessor = i % 10
        values(accessor - 1)
      } else ""
    }

    thousands + hundreds + tens + ones
  }
}