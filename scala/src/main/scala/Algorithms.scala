
object Algorithms {

  type Predicate[T] = (T, T) => Boolean

  //Using mutable array
  //complexity with n = arr.length: O(n^2)
  def statefulInsertionSort[T](pred: Predicate[T], arr: Array[T]): Array[T] = {
    for (j <-  1 until arr.length) { //executed n times
      val key = arr(j)
      var i = j - 1
      while ((i >= 0) && pred(key, arr(i))) { //executed at most i <= n times
        arr(i + 1) = arr(i)
        i = i - 1
      }
      arr(i + 1) = key
    }
    arr
  }


  //using immutable lists and pattern matching
  //Complexity:
  //There are n recursive calls to insertion sort every one of which make m < n recursive calls to insert
  //Complexity is therefore O(n^2)
  //we can prove it's correctness using induction where we always use insert on a sorted list
  //and then prove that insert always correctly places the element in the list
  def insertionSort[T](pred: (T, T) => Boolean, list: List[T]): List[T] = {
    def insert(elem: T, ls: List[T]): List[T] = {
      ls match {
        case Nil => elem :: Nil
        case x :: xs if pred(elem, x) => elem :: x :: xs
        case x :: xs => x :: insert(elem, xs) //recursive call to insert at most n times
      }
    }
    list match {
      case Nil => Nil
      case x :: xs => insert(x, insertionSort(pred, xs))// self recursive call executed n times
    }
  }

  // something is wrong here I spent already too much time trying to fix array indices and side effects
  // better start working on the good stuff instead
  def statefulMergeSort(array: Array[Int], start: Int, end: Int): Unit = {
    def merge(arr: Array[Int], p: Int, q: Int, r: Int): Unit = {
      val n1 = q - p
      val n2 = r - q
      println(arr.mkString(", "))
      val left: Array[Int] = arr.take(n1 + 1)
      val right: Array[Int] = arr.take(n2 + 1)
      println(left.mkString(", "))

      for (i <- 0 until n1) {
        left(i) = arr(p + i)
      }
      left(n1) = Int.MaxValue
      for (j <- 0 until n2) {
        right(j) = arr(q + j)
      }
      right(n2) = Int.MaxValue

      var i = 0
      var j = 0
      for (k <- p until r) {
        if (left(i) < right(j)) {
          arr(k) = left(i)
          i = i + 1
        } else {
          arr(k) = right(j)
          j = j + 1
        }
      }

    }
    if (start < end) {
      val mid: Int = Math.floor((start + end) / 2).toInt
      statefulMergeSort(array, start, mid)
      statefulMergeSort(array, mid + 1, end)
      merge(array, start, mid, end)
    }

  }

  //In this implementation we are using lazy stream to get O(1) split function
  def mergeSort[T](pred: Predicate[T], list: Stream[T]): Stream[T] = {
    def merge(lhs: Stream[T], rhs: Stream[T]): Stream[T] = {
      //if either list is empty return the other
      (lhs, rhs) match {
        case (Stream.Empty, r) => r
        case (l, Stream.Empty) => l
        case (l #:: ls, r #:: rs) =>
          // otherwise take the element that satisfies the predicate and
          // cons the rest of the list
          if (pred(l, r)) {
            l #:: merge(ls, rhs)
          } else {
            r #:: merge(lhs, rs)
          }
      }
    }

    // This is the hardest part, the split need to be O(1) but there is no way of
    // knowing the size of the list in O(1) time. Therefore
    // ls.take(n.length), ls.drop(n.length) is impractical
    // to go around this we'll make use of scala's lazy stream and define two new streams
    // using the function splt which returns a tuple of streams in terms
    // of the original stream
    //
    // This is a great case where functional programming without lazy evaluation
    // cannot cope in terms of performance with side effects and mutable data structures
    def split(ls: Stream[T]): (Stream[T], Stream[T]) = {
      def splt(ls: Stream[T], rs: Stream[T]): (Stream[T], Stream[T]) = {
        (ls, rs) match {
          case (x #:: xs, _ #:: _ #:: zs) =>
            val (us, vs) = splt(xs, zs)
            (x #:: us, vs)
          case (xs, _) => (Stream.empty, xs)
        }
      }
      splt(ls, ls)
    }

    list match {
      case Stream.Empty => Stream.Empty // the empty list is trivially sorted
      case x #:: Stream.Empty => list // the unary list is trivially sorted
      case ls => //the final list merges the split list recursively sorted
        val (left, right) = split(ls)
        merge(mergeSort(pred, left), mergeSort(pred, right))
    }
  }
}
