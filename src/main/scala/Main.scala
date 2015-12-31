
import Algorithms._

object Main {

  def main(args: Array[String]) = {
    val listToSort = List(4,3,2,1,0)
    val arrayToSort = Array(4,3,2,1,0)
    println(listToSort)
    println(mergeSort[Int](_ <= _, listToSort.toStream).mkString(", "))
  }
}
