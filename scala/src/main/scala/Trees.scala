
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

trait Vertex[T]

trait Weighted {
  def weight: Int
}

trait Discoverable {
  def discovered: Int
  def finished: Int
}
trait VertexState
case class Discovered(time: Int) extends VertexState
case class Finished(discoverTime: Int, finishTime: Int) extends VertexState
case object Unknown extends VertexState

class Edge[T](source: Vertex[T], destination: Vertex[T])

class DiscoverableEdge[T](val source: DiscoverableVertex[T], val destination: DiscoverableVertex[T])

class DiscoverableVertex[T](val outEdges: List[DiscoverableEdge[T]], var state: VertexState) extends Vertex[T]

class Graph[T](edges: Set[Edge[T]], verticies: Set[Vertex[T]])

class WeightedEdge[T](val source: Vertex[T], val destination: Vertex[T], w: Int) extends Edge[T](source, destination) with Weighted {
  override def weight: Int = w
  def toEdge = new Edge(source, destination)
}

class WeightedGraph[T](edges: Set[WeightedEdge[T]], verticies: Set[Vertex[T]]) extends Graph[T](edges.map(_.toEdge), verticies) with Weighted {
  def weight = edges.map(_.weight).sum
}
