
object Heap {

  def buildMaxHeapStateful(a: Array[Int]): Unit = {
    for (i <- (a.length/2 - 1) to 0 by -1) {
      maxHeapify(a, i)
    }
  }

  def maxHeapify(arr: Array[Int], i: Int): Unit = {
    val l = leftIndex(i)
    val r = rightIndex(i)
    var largest = if (l < arr.length && arr(l) > arr(i)) {
      l
    } else {
      i
    }
    if (r < arr.length && arr(r) > arr(largest)) {
      largest = r
    }

    if (largest != i) {
      swap(arr, i, largest)
      maxHeapify(arr, largest)
    }
  }

  def swap[T](a: Array[T], i: Int, j: Int): Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def leftIndex(i: Int): Int = 2 * (i + 1) - 1
  def rightIndex(i: Int): Int = 2 * (i + 1)
  def parentIndex(i: Int): Int = Math.floor(i/2).toInt

  def left(arr: Array[Int], i: Int): Int = arr(leftIndex(i))
  def right(arr: Array[Int], i: Int): Int = arr(rightIndex(i))
  def parent(arr: Array[Int], i: Int): Int = arr(parentIndex(i))

  /*
   * Heaps are not directly portable in a functional setting.
   * Instead we are going to use leftist heaps, to this end, we define a data
   * type that looks like this:
   * type Heap = Empty + (Int * Elem * Heap * Heap)
   * Where the product is the rank, element and children of a node in the heap
   * note that this is x = 1 + (T * x^2) which is the typical signature of a
   * binary tree. Therefore we could imagine using a zipper data structure too
   * to update it's value using persitent trees.
   *
   * Delete Root wasn't implemented because of heaps are contravariants and
   * ordering is invariant.
   */
   sealed trait Heap[+T] {
     def rank(): Int
     def empty(): Heap[T] = EmptyHeap
     def isEmpty(): Boolean
     def root(): T
     def insert[S >: T](elem: S)(implicit ord: Ordering[S]): Heap[S]
     def merge[S >: T](heap: Heap[S])(implicit ordering: Ordering[S]): Heap[S]
    //  def deleteRoot(implicit ordering: Ordering[T]): Heap[T]
   }

   def makeNode[T](e: T): Heap[T] = HeapNode(1, e, EmptyHeap, EmptyHeap)

   def makeT[T](elem: T, a: Heap[T], b: Heap[T]): Heap[T] =
     if (a.rank() >= b.rank) {
       HeapNode(b.rank() + 1, elem, a, b)
     } else {
       HeapNode(a.rank() + 1, elem, b, a)
     }

   case object EmptyHeap extends Heap[Nothing] {
     def isEmpty = true
     def rank = 0
     def root = throw new NoSuchElementException("Can't take the root of empty heap")
     def insert[S](elem: S)(implicit ord: Ordering[S]): Heap[S] = {
       println("inserting element " + elem)
       makeNode(elem)
     }
     def merge[S](heap: Heap.Heap[S])(implicit ordering: Ordering[S]): Heap[S] = heap
    //  def deleteRoot(implicit ordering: Ordering[T]): Heap.Heap[T] = throw new NoSuchElementException("Can't delete root of empty heap")
   }

   case class HeapNode[T](r: Int, e: T, left: Heap[T], right: Heap[T]) extends Heap[T] {
     def isEmpty = false
     def rank = r
     def root = e
     def insert[S >: T](elem: S)(implicit ord: Ordering[S]): Heap[S] = {
       println(s"inserting $elem into $this")
       this.merge[S](makeNode(e))(ord)
     }
     def merge[S >: T](heap: Heap[S])(implicit  ord: Ordering[S]): Heap[S] = heap match {
       case EmptyHeap => this
       case h2 @ HeapNode(_, y, a2, b2) =>
       println(s"merge node ${e} and ${h2.e}")
        if(ord.lteq(e, y)) {
          makeT(e, left, right.merge[S](h2)(ord))
        } else {
          makeT(y, a2, this.merge[S](b2)(ord))
        }
     }
    //  def deleteRoot(implicit ordering: Ordering[T]) = left.merge(right)(ordering)
   }

   def printMaxHeap[T](heap: Heap[T]): String = heap match {
     case EmptyHeap => ""
     case HeapNode(_, elem, lft, rgt) => s"$elem ${printMaxHeap(lft)} ${printMaxHeap(rgt)}"
   }
  def buildMaxHeap[T](list: List[T])(implicit ord: Ordering[T]): Heap[T] =
    list.foldLeft(EmptyHeap: Heap[T])((heap, elem) => heap.insert(elem))

}
