
/*
 * double linked lists are impossible in a stateless setting, but zippers on lists
 * are close enough
 */
case class Zipper[T](left: List[T], e: T, right: List[T]) extends DoubleLinkedList[T] {
  def isEmpty = false
  def next = Zipper(left.tail, left.head, e :: right)
  def prev = Zipper(e :: left, right.head, right.tail)
  def head = e
  def insert[S >: T](t:S) = insertBefore(t)
  def insertAfter[S >: T](t: S): DoubleLinkedList[S] = Zipper(e :: left, t,right)
  def insertBefore[S >: T](t: S): DoubleLinkedList[S] = Zipper(left, t, e :: right)
}

trait DoubleLinkedList[+T] {
  def isEmpty: Boolean
  def head: T
  def insert[S >: T](t: S): DoubleLinkedList[S]
}
//had to ditch variance to comply with insert and remove
trait MutableDLL[T] {
  def isEmpty: Boolean
  def head: Option[T]
  def insert(t: T): Unit
  def remove(e: T): Unit
}

//The actual mutable linked list took much more work than the stateless one
class MutableLinkedList[T] extends MutableDLL[T] {

  sealed trait LLNode

  case class LLSentinel(var pointer: LLNode) extends LLNode

  case class LLElem(var prev: LLNode, elem: T, var next: LLNode) extends LLNode

  val start = LLSentinel(null)
  val end = LLSentinel(start)
  start.pointer = end

  override def head: Option[T] = start.pointer match {
    case s: LLSentinel => None
    case LLElem(_, e, _) => Some(e)
  }

  def isEmpty = start.pointer == end

  def insert(e: T): Unit = {
    val n = LLElem(null, e, null)
    val nxt = start.pointer
    val lst = end.pointer
    n.prev = lst
    n.next = nxt
    nxt match {
      case s: LLSentinel => s.pointer = n
      case elem: LLElem => elem.prev = n
    }
    lst match {
      case s: LLSentinel => s.pointer = n
      case elem: LLElem => elem.next = n
    }
    this
  }

  def remove(e: T): Unit = {
    this.find(e).foreach( (elem: LLElem) => {
      val previous = elem.prev
      val next = elem.next
      previous match {
        case s: LLSentinel => s.pointer = elem.next
        case e: LLElem => e.next = elem.next
      }
      next match {
        case s: LLSentinel => s.pointer = elem.prev
        case e: LLElem => e.prev = elem.prev
      }

    }
    )

  }
  def find(t: T): Option[LLElem] = {

    def loop(node: LLNode): Option[LLElem] = node match {
      case elem @ LLElem(_, e, n) => if (e == t) Some(elem) else loop(n)
      case _ => None
    }
    loop(start.pointer)
  }
}