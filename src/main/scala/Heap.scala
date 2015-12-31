
object Heap {

  def buildMaxHeap(a: Array[Int]): Unit = {
    for (i <- (a.length/2 - 1) to 0 by -1) {
      maxHeapify(a, i)
    }
  }
  def maxHeapify(arr: Array[Int], i: Int): Unit = {
    println("max heapify of " + arr.mkString(", ") + " at index " +i)
    val l = leftIndex(i)
    val r = rightIndex(i)
    println("left is " + l)
    println("right is " + r)
    println("length is " + arr.length)
    val largest = if (l < arr.length && arr(l) > arr(i)) {
      l
    } else if (r < arr.length && arr(r) > arr(i)) {
      r
    } else {
      i
    }
    if (largest != i) {
      swap(arr, i, largest)
      maxHeapify(arr, largest)
    }
  }

  def swap[T](a: Array[T], i: Int, j: Int): Unit = {
    println("before swap: " + a.mkString(", "))
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
    println("after swap: " + a.mkString(", "))

  }

  def leftIndex(i: Int): Int = 2 * (i + 1) - 1
  def rightIndex(i: Int): Int = 2 * (i + 1)
  def parentIndex(i: Int): Int = Math.floor(i/2).toInt

  def left(arr: Array[Int], i: Int): Int = arr(leftIndex(i))
  def right(arr: Array[Int], i: Int): Int = arr(rightIndex(i))
  def parent(arr: Array[Int], i: Int): Int = arr(parentIndex(i))
}
