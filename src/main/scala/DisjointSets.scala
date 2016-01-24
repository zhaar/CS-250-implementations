// trait ArbitrarySet[T] {
//   def add(elem: T): ArbitrarySet
// }
//
// case class DisjointSet[T](representative: T, associated: List[T]) extends ArbitrarySet[T] {
//   def add(elem: T) = this.clone(associated = elem :: associated)
// }
