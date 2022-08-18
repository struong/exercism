import cats.data.State

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

final case class Seed(long: Long) {
  def next: Seed = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

object Generator {
  val cachedNames: mutable.Set[String] = mutable.Set.empty
  var seed: Seed = Seed(42)

  @tailrec
  def createUniqueName: String = {
    seed = seed.next
    val currentName = makeName(seed)
    if (cachedNames.contains(currentName)) {
      createUniqueName
    } else {
      cachedNames += currentName
      currentName
    }
  }

  def randomChar(seed: Seed): Char = (new Random(seed.long).nextInt(26) + 65).toChar

  def randomInt(seed: Seed): Int = new Random(seed.long).nextInt(10)

  def nextChar: State[Seed, Char] = State(seed =>
    (seed.next, randomChar(seed))
  )

  def nextInt: State[Seed, Int] = State(seed =>
    (seed.next, randomInt(seed))
  )

  def makeName(seed: Seed): String = {
    def generateName: State[Seed, String] = {

      for {
        c1 <- nextChar
        c2 <- nextChar
        i1 <- nextInt
        i2 <- nextInt
        i3 <- nextInt
      } yield {
        s"$c1$c2$i1$i2$i3"
      }
    }

    generateName.runA(seed).value
  }
}

class Robot {
  var name: String = Generator.createUniqueName

  def reset(): Unit = name = Generator.createUniqueName
}
