trait ArbitrarySet[+T] {
  def add[S >: T](elem: S): ArbitrarySet[S]
}

case class DisjointListSet[T](representative: T, associated: List[T]) extends ArbitrarySet[T] {
  def add[S >: T](elem: S) = this.copy(associated = elem :: associated)
  def find(elem: T) = if (associated.contains(elem)) Some(representative) else None
  def union(that: DisjointListSet[T]): DisjointListSet[T] = DisjointListSet(
    representative,
    if (this.associated.length < that.associated.length)
      this.associated ::: that.associated
    else
      that.associated ::: this.associated)
}
//
// case class DisjointTreeSet[T](private val nodes: Map[T, DisjoinTreeNode]) {
//   def add(elem: T): Unit = this union DisjointTreeNode(elem, None)
//   def find(elem: T): Unit = nodes.get(elem).map(_.representative)
//   def union(that: DisjoinTreeSet[T]) = if (this.rank > that.rank){
//     that.parent = Some(this)
//     nodes += (that.value -> that)
//   }else {
//       this.parent = some(that)
//       if (this.rank == that.rank) that.rank = that.rank + 1
//     }
// }

object DisjointTreeSet {
  def findSet[T](x: DisjointTreeNode[T]): DisjointTreeNode[T] = x.representative
  def link[T](x: DisjointTreeNode[T], y: DisjointTreeNode[T]): Unit = if (x.rank > y.rank){
    y.parent = Some(x)
  }else {
      x.parent = Some(y)
      if (x.rank == y.rank) y.rank = y.rank + 1
    }
}

case class DisjointTreeNode[T](value: T, var parent: Option[DisjointTreeNode[T]], var rank: Int = 0) {
  def representative: DisjointTreeNode[T] = if (parent.isDefined) {
    this.parent = Some(parent.get.representative)
    this.parent.get
  } else {
    this
  }
}
