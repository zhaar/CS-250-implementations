
class MutableTree[T](edges: Set[DiscoverableEdge[T]], verticies: Set[DiscoverableVertex[T]]) {
  def DFS(): Unit = {
    var time = 0;
    def visit(vertex: DiscoverableVertex[T]): Unit = {
      time = time + 1
      val discoveryTime = time
      vertex.state = Discovered(discoveryTime)
      vertex.outEdges.foreach(e => {
        val d = e.destination
        if (d.state == Unknown) {
          visit(d)
        }
      })
      time = time + 1
      vertex.state = Finished(discoveryTime, time)
    }
    for (v <- verticies) {
      v.state = Unknown
    }
    verticies.foreach { v =>
      if (v.state == Unknown) {
        visit(v)
      }
    }
  }
}
