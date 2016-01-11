
import Algorithms._
import MaxSubArray.maxSubArray
import Heap._
object Main {

  def printArray[T](arr: Array[T]): String = arr.mkString(", ")

  def main(args: Array[String]) = {
    val listToSort = List(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val arrayToSort = Array(4,3,2,1,0)
    println(listToSort)
    println(mergeSort[Int](_ <= _, listToSort.toStream).mkString(", "))
    val maxSub = Array(-2, -4, 3, -1, 5, 7, -7, -1)
    println(maxSubArray(maxSub))

    val heapArray = Array(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    println("array to heapify: " + printArray(heapArray))
    buildMaxHeapStateful(heapArray)
    println("after heapification: " + printArray(heapArray))


    val heap = buildMaxHeap(listToSort)
  println("functional heapification: " + printMaxHeap(heap))
    }
}
