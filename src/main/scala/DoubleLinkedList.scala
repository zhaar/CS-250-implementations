
/*
* A double linked list is a list with each node having a pointer to it's next and previous neighbor
* algebraically we could define it like so: L = 1 + (L * T * L)
* so it's either an empty list or a triple of a List, an element and another List
* The problem with this implementation is that we don't make use of persistent data and
* our insert function will then run in O(n) which we do not want.
* A better solution would be to use some sort of zipper with would look like this:
* 1 +
*/
trait DoubleLinkedList[+T] {
  def isEmpty: Boolean
  def next: DoubleLinkedList[T]
  def prev: DoubleLinkedList[T]
  def elem: T
  def insertAfter[S >: T](t: S): DoubleLinkedList[S]
  def insertBefore[S >: T](t: S): DoubleLinkedList[S]
}

case object Empty extends DoubleLinkedList[Nothing] {
  def isEmpty = true
  def next = throw new NoSuchElementException("Empty has no next element")
  def prev = throw new NoSuchElementException("Empty has no previous element")
  def elem = throw new NoSuchElementException("Empty has no value")
  def insertAfter[T](t: T) = Node(Nil, t, Nil)
  def insertBefore[T](t: T) = Node(Nil, t, Nil)
}

case class Node[T](left: List[T], e: T, right: List[T]) extends DoubleLinkedList[T] {
  def isEmpty = false
  def next = Node(left.tail, left.head, e :: right)
  def prev = Node(e :: left, right.head, right.tail)
  def elem = e
  def insertAfter[S >: T](t: S): DoubleLinkedList[S] = Node(e :: left, t,right)
  def insertBefore[S >: T](t: S): DoubleLinkedList[S] = Node(left, t, e :: right)
}

object DoubleLinkedList {
  def map[T, S](list: DoubleLinkedList[T], f: T => S): DoubleLinkedList[S] = list match {
    case Empty => Empty
    case Node(p, e, n) => Node(p map f, f(e), n map f)
  }
}
