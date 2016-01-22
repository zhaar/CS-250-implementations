object RodCutting {

  //Topdown
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
  // def memoized_cut_rod_aux(p, n, r):
  //   if r[n] >= 0:
  //       return r[n] #Return directly if we already calculated the value
  //   best = -1
  //   if n == 0:
  //       best = 0
  //   else:
  //       for i in range(1, n+1):
  //           best = max(best, p[i] + memoized_cut_rod_aux(p, n-i, r))
  //   r[n] = best
  //   return r[n]
//
//
// def memoized_cut_rod(p, n):
//     r = [-1]*(n+1) #Create an array r[0...n]
//     return memoized_cut_rod_aux(p, n, r)

  /*
   * making this algorithm stateless is not quite trivial since we
   * don't have access to a random access array
   * but using bottom up wll help us
   * TODO: each recusrive call to loop concatenates a new element to the cache
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
