import P70.string2MTree
import multiwaytree._

object P73 extends App {
  implicit class MTreeOps[T](val tree: MTree[T]) {
    def listpyTree: String = {
      if(tree.children.isEmpty)
        tree.value.toString
      else
        s"(${tree.value} ${tree.children.map(_.listpyTree).mkString(" ")})"
    }
  }

  println("MTree(\"a\", List(MTree(\"b\", List(MTree(\"c\"))))).listpyTree = " +
           MTree("a", List(MTree("b", List(MTree("c"))))).listpyTree)

  println("string2MTree(\"afg^^c^bd^e^^^\").listpyTree = " +
           string2MTree("afg^^c^bd^e^^^").listpyTree)
}
