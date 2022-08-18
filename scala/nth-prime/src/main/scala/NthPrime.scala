import scala.annotation.tailrec

object NthPrime {
  // 5,000,000th prime
  val limit = 86028121

  def prime(n: Int): Option[Int] = {
    if (n == 0) None
    else Some(sieveOfAtkin(n))
  }

  def sieveOfAtkin(n: Int): Int = {
    val sieveList: Array[Boolean] = Array.fill(limit + 1)(false)
    if (limit > 2)
      sieveList(2) = true
    if (limit > 3)
      sieveList(3) = true
      
    @tailrec
    def sieve(x: Int, limit: Int): Unit = {
      if (x * x <= limit) {
        conditions(1, x, limit)
        sieve(x + 1, limit)
      }
    }

    @tailrec
    def conditions(y: Int, x: Int, limit: Int): Unit = {
      if (y * y <= limit) {
        val condition1 = (4 * x * x) + (y * y)
        if (
          condition1 <= limit && (condition1 % 12 == 1 || condition1 % 12 == 5)
        ) {
          sieveList(condition1) ^= true // flips the sieve
        }
        val condition2 = (3 * x * x) + (y * y)
        if (condition2 <= limit && condition2 % 12 == 7) {
          sieveList(condition2) ^= true
        }
        val condition3 = (3 * x * x) - (y * y)
        if (x > y && condition3 <= limit && condition3 % 12 == 11) {
          sieveList(condition3) ^= true
        }
        conditions(y + 1, x, limit)
      }
    }

    // create booleans
    sieve(1, limit)

    // mark all multiples of squares as non-prime
    @tailrec
    def squares(i: Int, limit: Int): Unit = {
      if (i * i <= limit) {
        if (sieveList(i)) {
          Range(i * i, limit, i * i).foreach(r => sieveList(r) = false)
        }
        squares(i + 1, limit)
      }
    }

    squares(5, limit)

    val primes =
      sieveList.zipWithIndex.filter(p => p._1 == true).map(p => p._2)

    primes(n - 1)
  }
}
