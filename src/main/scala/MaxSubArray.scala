import scala.annotation.tailrec

object MaxSubArray {

  //This implementation is mostly fine except for the crossing section
  // there is an indexing mistake I'm not going to bother fixing now
  def statefulMaxSubArray(array: Array[Int], low: Int, high: Int): (Int, Int, Int) = {
    def maxCrossing(low: Int, mid: Int, high: Int): (Int, Int, Int) = {
      var left_max = 0
      var right_max = 0
      var left_sum = 0
      var right_sum = 0
      var currentSum = 0
      for (i <- mid to 0 by -1) {
        currentSum = currentSum + array(i)
        if (currentSum > left_sum) {
          left_sum = currentSum
          left_max = i
        }
      }
      currentSum = 0
      for (i <- (mid + 1) until high) {
        currentSum = currentSum + array(i)
        if (currentSum > right_sum) {
          right_sum = currentSum
          right_max = i
        }
      }
      (left_max, right_max, left_sum + right_sum)
    }

    if (high == low) {
      (low, high, array(low))
    } else {
      val mid: Int = Math.floor((low + high) / 2).toInt
      val left @ (_, _, left_sum) = statefulMaxSubArray(array, low, mid)
      val right @ (_, _, right_sum) = statefulMaxSubArray(array, mid + 1, high)
      val cross @ (_, _, cross_sum) = maxCrossing(low, mid, high)
      if (left_sum >= right_sum && left_sum >= cross_sum) {
        left
      } else if (right_sum >= left_sum && right_sum >= cross_sum) {
        right
      } else {
        cross
      }
    }
  }

  // Not much to see here except for the maximum crossing value.
  // this algorithm only uses state to traverse the two sub arrays
  // The main difference is the use of an immutable Seq instead of array and a tailrec function
  // to compute the maximum crossing value
  def maxSubArray(array: Seq[Int]): (Int, Int, Int) = {
    def maxCrossing(low: Int, mid: Int, high: Int): (Int, Int, Int) = {
      //I got to a point where writing tailrec functions is both more natural and more reliable than loops
      @tailrec
      def traverseAndGetMax(currentSum: Int, currentMax: Int, maxIndex: Int, currentIndex: Int, endPred: (Int) => Boolean, iter: (Int) => Int ): (Int, Int) = {
        if (endPred(currentIndex)) {
          (maxIndex, currentMax)
        } else {
          val sum = currentSum + array(currentIndex)
          if (sum > currentMax) {
            traverseAndGetMax(sum, sum, currentIndex, iter(currentIndex), endPred, iter)
          } else {
            traverseAndGetMax(sum, currentMax, maxIndex, iter(currentIndex), endPred, iter)
          }
        }
      }

      val (lowIndex, lowSum) = traverseAndGetMax(0, 0, mid, mid, _ < 0, _ - 1)
      val (highIndex, highSum) = traverseAndGetMax(0, 0, mid + 1, mid + 1, _ > high, _ + 1)
      (lowIndex, highIndex, lowSum + highSum)
    }

    def maximumSubArray(low: Int, high: Int): (Int, Int, Int) = {
      if (high == low) {
        (low, high, array(low))
      } else {
        val mid: Int = Math.floor((low + high) / 2).toInt
        val left @ (_, _, left_sum) = maximumSubArray(low, mid)
        val right @ (_, _, right_sum) = maximumSubArray(mid + 1, high)
        val cross @ (_, _, cross_sum) = maxCrossing(low, mid, high)
        if (left_sum >= right_sum && left_sum >= cross_sum) {
          left
        } else if (right_sum >= left_sum && right_sum >= cross_sum) {
          right
        } else {
          cross
        }
      }
    }

    maximumSubArray(0, array.length - 1)
  }
}
