
trait ArbitraryTree[+T] {
  def rank: Int
}

case object Leaf extends ArbitraryTree[Nothing] {
  def rank: Int = 0
}

trait BTree[T] extends ArbitraryTree[T]

case class BinaryTree[T](elem: T, left: BTree[T], right: BTree[T]) extends BTree[T] {
  def rank: Int = 1 + Math.max(left.rank, right.rank)
}

case class AnyTree[T](elem: T, children: List[AnyTree[T]]) extends ArbitraryTree[T] {
  def rank = 0
}

trait Vertex[T] {
  def outEdges: List[Edge[T]]
}

trait weighted {
  def weight: Int
}

abstract case class Edge[T](source: Vertex[T], destination: Vertex[T])

abstract case class Graph[T](edges: Set[Edge[T]], verticies: Set[Vertex[T]]) {
  def kruskal: AnyTree[T] = ???
  def prims: AnyTree[T] = ???
  def depthFirstSearch(source: Edge[T]): List[(Edge[T], Int)] = {
    ???
  }
}
