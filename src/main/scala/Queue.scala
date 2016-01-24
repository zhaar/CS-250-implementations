trait AbstractQueue[+T] {
  def enqueue[S >: T](e: S): AbstractQueue[S]
  def dequeue: (T, AbstractQueue[T])
  def map[S](f: T => S): AbstractQueue[S]
}

/*
 * first implementation uses the fact that an queue is a start element, an
 * end element and a queue in between.
 * sadly, this implementation doesn't conform to the traditionnal running time of
 * queue operation that are O(1) enqueue and dequeue
 */
case object EmptyQueue extends AbstractQueue[Nothing] {
  def enqueue[T](e: T) = AbstractQueue.SingletonQueue(e)
  def dequeue = throw new NoSuchElementException("can't dequeue and empty queue")
  def map[S](f: Nothing => S): AbstractQueue[Nothing] = EmptyQueue
}

case class InefficientQueue[T](first: T, mid: AbstractQueue[T], last: T) extends AbstractQueue[T] {
  def enqueue[S >: T](e: S): InefficientQueue[S] = InefficientQueue(first, mid.enqueue(last), e)
  def dequeue = (first , mid match {
    case EmptyQueue => AbstractQueue.SingletonQueue(last)
    case InefficientQueue(fst, EmptyQueue, lst) => InefficientQueue(fst, AbstractQueue.SingletonQueue(lst), last)
    case InefficientQueue(fst, md, lst) => InefficientQueue(fst, md.enqueue(lst), last)
  })
  def map[S](f: T => S) = InefficientQueue(f(first), mid map f, f(last))
}

object AbstractQueue {
  def SingletonQueue[T](e: T): InefficientQueue[T] = InefficientQueue[T](e, EmptyQueue, e)
}

/*
 * to get O(1) enqueue and dequeue we are going to use amortized analysis
 * with the banker method and use a really easy and naive implementation that is:
 * a queue is two lists, one with equeued elements, and one with elements to dequeue
 * once the dequeue list is empty, reverse the enqueued list and use it as the new
 * dequeue list, the new enqueue list is now empty
 */
 case class Queue[T](deq: List[T], enq: List[T]) extends AbstractQueue[T] {
   def enqueue[S >: T](e: S): Queue[S] = Queue(deq, e :: enq)
   def dequeue = deq match {
     case Nil => Queue(enq.reverse, Nil).dequeue
     case x :: xs => (x, Queue(xs, enq))
   }
   def map[S](f: T => S) = Queue(deq map f, enq map f)
 }
