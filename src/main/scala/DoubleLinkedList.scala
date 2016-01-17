
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
  def insertAfter(t: T): DoubleLinkedList
  def insertBefore(t: T): DoubleLinkedList
}

case object Empty extends DoubleLinkedList[Nothing] {
  def isEmpty = true
  def next = throw new NoSuchElementException
  def prev = throw new NoSuchElementExcpetion
  def elem = throw new NoSuchElementException
  def insertAfter(t: T) = Node(Nil, t, Nil)
  def insertBefore(t: T) = Node(Nil, t, Nil)
}

case class Node[T](left: List[T], e: T, right: List[T]) extends DoubleLinkedList[T] {
  def isEmpty = false
  def next = Node(left.tail, left.head, e :: right)
  def prev = Node(e :: left, right.head, right.tail)
  def elem = e
  def insertAfter(t: T): DoubleLinkedList = Node(e :: left, t,right)
  def insertBefore(t: T): DoubleLinkedList = Node(left, t, e :: right)
}
