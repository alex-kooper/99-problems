import multiwaytree.MTree

object P70C extends App {

  implicit class MTreeOps[T](tree: MTree[T]) {
    def nodeCount: Int = {
      tree.children.view.map(_.nodeCount).sum + 1
    }
  }

  println("Problem P70C")
  println("____________")

  print("MTree('a', List(MTree('f'))).nodeCount = ")
  println(MTree('a', List(MTree('f'))).nodeCount)

  print("MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).nodeCount = ")
  println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).nodeCount)
}
