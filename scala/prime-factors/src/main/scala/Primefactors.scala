import scala.annotation.tailrec
object PrimeFactors {

  def factors(n: Long): List[Long] = {
    @tailrec
    def findPrimes(remaining: Long, divisor: Long, primes: List[Long]): List[Long] =
      remaining match {
        case _ if remaining == 1 => primes
        case _ if remaining % divisor == 0 =>
          findPrimes(remaining / divisor, 2, primes :+ divisor)
        case _ => findPrimes(remaining, divisor + 1, primes)
      }

    findPrimes(n, 2, List.empty)
  }
}
