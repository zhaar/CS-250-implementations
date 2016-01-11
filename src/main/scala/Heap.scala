
object Heap {

  def buildMaxHeapStateful(a: Array[Int]): Unit = {
    for (i <- (a.length/2 - 1) to 0 by -1) {
      maxHeapify(a, i)
    }
  }

  def maxHeapify(arr: Array[Int], i: Int): Unit = {
    println("max heapify of " + arr.mkString(", ") + " at index " +i)
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
    println(s"swapping a[$i] = ${a(i)} and a[$j] = ${a(j)}")
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
   * Heaps are not directly modelable in a functional setting.
   * We are going to need to define a data type that looks like this:
   * type Heap = Empty + (Int * Elem * Head * Heap)
   * Where the product is the rank, element and children of a node in the heap
   * note that this is x = 1 + (T * x^2) which is the typical signature of a
   * binary tree. Therefore we could imagine using a zipper data structure too
   * to update it's value using persitent trees.
   */
   sealed trait Heap[T] {
     def rank(): Int
     def empty(): Heap[T] = EmptyHeap
     def isEmpty(): Boolean
     def root(): T
     def insert(elem: T): Heap[T]
     def merge(heap: Heap[T])(implicit ordering: Ordering[T]): Heap[T]
     def deleteRoot(implicit ordering: Ordering[T]): Heap[T]
   }

   def make[T](e: T): Heap[T] = HeapNode(1, e, EmptyHeap, EmptyHeap)

   def link[T](elem: T, lhs: Heap[T], rhs: Heap[T]): Heap[T] =
     if (lhs.rank() < rhs.rank) {
       HeapNode(lhs.rank() + 1, elem, rhs, lhs)
     } else {
       HeapNode(rhs.rank() + 1, elem, lhs, rhs)
     }

   case object EmptyHeap extends Heap {
     def isEmpty = true
     def rank = 0
     def root = throw new NoSuchElementException("Can't take the root of empty heap")
     def insert[T](elem: T): Heap[T] = make(elem)
     def merge[T](heap: Heap.Heap[T])(implicit ordering: Ordering[T]): Heap.Heap[T] = heap
     def deleteRoot(implicit ordering: Ordering[T]): Heap.Heap[T] = throw new NoSuchElementException("Can't delete root of empty heap")
   }

   case class HeapNode[T](r: Int, elem: T, left: Heap[T], right: Heap[T]) extends Heap[T] {
     def isEmpty = false
     def rank = r
     def root = elem
     def insert(e: T)(implicit ord: Ordering[T]): Heap[T] = make(e).merge(this)(ord)
     def merge(heap: Heap[T])(implicit  ord: Ordering[T]): Heap[T] = (this, heap) match {
       case (h: Heap[T], EmptyHeap[T]) => h
       case (h1 @ HeapNode(_, e1, lft1, rgt1), h2 @ HeapNode(_, e2, lft2, rgt2)) =>
        if(ord.lt(e1, e2)) {
          link(e1, lft1, rgt1.merge(h2)(ord))
        } else {
          link(e2, lft2, h1.merge(rgt2)(ord))
        }
     }
     def deleteRoot(implicit ordering: Ordering[T]) = left.merge(right)(ordering)
   }

  def buildMaxHeap[T](list: List[T]): Heap[T] =
    list.foldLeft(EmptyHeap: Heap[T])((heap, elem) => heap.insert(elem))

}
