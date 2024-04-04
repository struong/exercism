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
