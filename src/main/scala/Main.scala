
import Algorithms._
import MaxSubArray.maxSubArray

object Main {

  def main(args: Array[String]) = {
    val listToSort = List(4,3,2,1,0)
    val arrayToSort = Array(4,3,2,1,0)
    println(listToSort)
    println(mergeSort[Int](_ <= _, listToSort.toStream).mkString(", "))
    val maxSub = Array(-2, -4, 3, -1, 5, 7, -7, -1)
    println(maxSubArray(maxSub))
  }
}
