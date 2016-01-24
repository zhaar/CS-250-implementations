import org.scalatest.FlatSpec

import DynamicProgramming._

class DPSpec extends FlatSpec {
  "The fourth number of fibonacci sequence" should "be 3" in {
    assert(fibonacci(4) == 3)
  }

  "RodCutting problem" should "work statefully" in {
    val rodPrices = Array(0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30)
    assert(memoizedRodCut(rodPrices) == 23))
  }

  it should "work statelessly as well" in {
    val rodPrices = Array(0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30)
    assert(statelessRodCut(rodPrices.toSeq))
  }
}
