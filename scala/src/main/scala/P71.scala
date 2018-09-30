import multiwaytree._

object P71 extends App {
  import P70._

  implicit class MTreeOps[T](val tree: MTree[T]) {
    def internalPathLength: Int = findAllPaths.map(_.length - 1).sum

    type Path = List[MTree[T]]

    def findAllPaths: List[Path] = {
      tree.children.flatMap(child => addParent(tree, child, child.findAllPaths))
    }

    private[this] def addParent(parent: MTree[T], child: MTree[T], childPaths: List[Path]): List[Path] =
      List(parent, child) :: childPaths.map(parent :: _)
  }

  println("Problem P71")
  println("____________")

  println("string2MTree(\"afg^^c^bd^e^^^\").internalPathLength = " +
           string2MTree("afg^^c^bd^e^^^").internalPathLength)
}
