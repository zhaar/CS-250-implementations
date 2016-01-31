object DynamicProgramming {

  //since fibonacci is a very simple recursive function we can define it as a
  //lazy stream
  def fibonacci(n: Int): Int = {
    def f: Stream[Int] = 0 #:: 1 #:: f.zip(f.tail).map {case (fst, snd) => fst + snd}
    f.drop(n).head
  }

  //Topdown stateful
  def memoizedRodCut(prices: Array[Int]): Int = {
    val cache = prices map { _ => -1 }
    var cutposition = prices map { _ => 0 }
    def loop(length: Int): Int = {
      if (cache(length) >= 0) {
        return cache(length)
      }
      val best = if (length == 0) {
        0
      } else {
        ((1 to length) map { i =>
          prices(i-1) + loop(length - i)
        }).max
      }
      cache(length) = best
      return best
    }
    return loop(prices.length - 1)
  }

  /*
   * making this algorithm stateless is not quite trivial since we
   * don't have access to a random access array
   * but using bottom up wll help us
   * TODO: each recursive call to loop concatenates a new element to the cache
   * we need to make sure this call doesn't run in O(n)
   */
   def statelessRodCut(prices: Seq[Int]): Int = {
     def loop(cache: Seq[Int], index: Int): Int = {
       if (index == prices.length) {
         cache(index - 1)
       } else {
         val maxProfit = ((1 to index) map {i => prices(i - 1) + cache(index - i)}).max
         loop(cache :+ maxProfit, index + 1)
       }
     }
     loop(Seq(0), 1)
   }
}
