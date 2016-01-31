trait ArbitrarySet[T] {
  def add(elem: T): ArbitrarySet[T]
}

case class DisjointSet[T](representative: T, associated: List[T]) extends ArbitrarySet[T] {
  def add(elem: T) = this.copy(associated = elem :: associated)
}
