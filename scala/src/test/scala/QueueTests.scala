import org.scalatest.FlatSpec

import Queue._

class QueueSpec extends FlatSpec {
  "A queue" should "enqueue and dequeue elements in fifo order" in {
    val queue = Queue(Nil, Nil).enqueue(1).enqueue(2).enqueue(3)
    val (one, r1) = queue.dequeue
    val (two, r2) = r1.dequeue
    val (three, empty) = r2.dequeue
    assert(one == 1)
    assert(two == 2)
    assert(three == 3)
  }

  it should "map every element correctly" in {
    val queue = Queue(Nil, Nil).enqueue(1).enqueue(2).enqueue(3) map {_ + 1}
    val (two, r1) = queue.dequeue
    val (three, r2) = r1.dequeue
    val (four, empty) = r2.dequeue
    assert(two == 2)
    assert(three == 3)
    assert(four == 4)
  }
}
