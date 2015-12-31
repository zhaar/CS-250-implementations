
import Algorithms._
import MaxSubArray.maxSubArray
import Heap._
object Main {

  def main(args: Array[String]) = {
    val listToSort = List(4,3,2,1,0)
    val arrayToSort = Array(4,3,2,1,0)
    println(listToSort)
    println(mergeSort[Int](_ <= _, listToSort.toStream).mkString(", "))
    val maxSub = Array(-2, -4, 3, -1, 5, 7, -7, -1)
    println(maxSubArray(maxSub))

    val heapArray = Array(5,6,7,8,9,10,11,12,13,14,15)
    println("array to heapify: " + printArray(heapArray))
    buildMaxHeap(heapArray)
    println("after heapificatoin: " + printArray(heapArray))

    def printArray[T](arr: Array[T]): String = arr.mkString(", ")
  }
}
