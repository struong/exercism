import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

sealed trait Bst[+T] {
  def value: T
  // type widening due to ordering
  def insert[S >: T](value: S)(implicit ordering: Ordering[S]): Bst[S]
  def left: Bst[T]
  def right: Bst[T]
  def get: Bst[T]
  def size: Int
  def isEmpty: Boolean
  def isLeaf: Boolean
}
case object BEnd extends Bst[Nothing] {
  override def value: Nothing = throw new NoSuchElementException()

  override def insert[S >: Nothing](value: S)(implicit
      ordering: Ordering[S]
  ): Bst[S] = throw new NoSuchElementException()

  override def left: Bst[Nothing] = throw new NoSuchElementException()
  override def right: Bst[Nothing] = throw new NoSuchElementException()
  override def get: Bst[Nothing] = this
  override def size: Int = 0

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false
}

case class BstNode[+T](
    override val value: T,
    override val left: Bst[T],
    override val right: Bst[T]
) extends Bst[T] {
  override def insert[S >: T](newValue: S)(implicit
      ordering: Ordering[S]
  ): Bst[S] = {
    val compare = ordering.compare(newValue, value)
    compare match {
      case diff if diff <= 0 && left.isEmpty => this.copy(left = Bst(newValue))
      case diff if diff > 0 && right.isEmpty => this.copy(right = Bst(newValue))
      case diff if diff <= 0 => this.copy(left = left.insert(newValue))
      case _                 => this.copy(right = right.insert(newValue))
    }
  }

  override def get: Bst[T] = this

  override val size: Int = 1 + left.size + right.size

  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty
}

object Bst {
  def apply[T](value: T): Bst[T] = BstNode(value, BEnd, BEnd)
  def fromList[T](items: List[T])(implicit ordering: Ordering[T]): Bst[T] = {
    @tailrec
    def fromListTailrec(remaining: List[T], accum: Bst[T]): Bst[T] = {
      if (remaining.isEmpty) accum
      else {
        val value = remaining.head
        fromListTailrec(remaining.tail, accum.insert(value))
      }
    }

    fromListTailrec(items.tail, Bst(items.head))
  }

  def toList[T](tree: Bst[T]): List[T] = {
    def toListTailRec(node: Bst[T], acc: List[T]): List[T] =
      if (node.isEmpty) acc
      else
        toListTailRec(node.left, acc) ++ (node.value :: acc) ++ toListTailRec(
          node.right,
          acc
        )

    toListTailRec(tree, List())
  }
}

/** @version created manually * */
class BstTest extends AnyFlatSpec with Matchers {
  val bst4 = Bst(4)

  it should "retain data" in {
    bst4.value should equal(4)
  }

  it should "retain data - char" in {
    Bst('d').value should equal('d')
  }

  it should "insert less" in {
    bst4.insert(2).left.get.value should equal(2)
  }

  it should "insert less - char" in {
    Bst('d').insert('a').left.get.value should equal('a')
  }

  it should "insert same" in {
    bst4.insert(4).left.get.value should equal(4)
  }

  it should "insert greater than" in {
    bst4.insert(5).right.get.value should equal(5)
  }

  it should "stuff" in {
    println(bst4.insert(2).insert(6).insert(1))
  }

  it should "handle complex tree - sort out of order list" in {
    val bst = Bst.fromList(List(4, 2, 6, 1, 3, 7, 5))
    println(bst)
    Bst.toList(bst) should equal((1 to 7).toList)

    bst.value should equal(4)
    bst.left.get.value should equal(2)
    bst.left.get.left.get.value should equal(1)
    bst.left.get.right.get.value should equal(3)
    bst.right.get.value should equal(6)
    bst.right.get.left.get.value should equal(5)
    bst.right.get.right.get.value should equal(7)
  }

  it should "iterating one element" in {
    Bst.toList(bst4) should equal(List(4))
  }

  it should "iterating over smaller element" in {
    Bst.toList(Bst.fromList(List(4, 2))) should equal(List(2, 4))
  }

  it should "iterating over larger element" in {
    Bst.toList(Bst.fromList(List(4, 5))) should equal(List(4, 5))
  }

  it should "iterating over complex tree" in {
    Bst.toList(Bst.fromList(List(4, 2, 1, 3, 6, 7, 5))) should equal(
      (1 to 7).toList
    )
  }

  it should "iterating over complex tree - chars" in {
    Bst.toList(Bst.fromList(List('d', 'b', 'a', 'c', 'f', 'g', 'e'))) should
      equal(('a' to 'g').toList)
  }
}
