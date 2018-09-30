import multiwaytree._
import P70.string2MTree

object P72 extends App {
  implicit class MTreeOps[+T](val tree: MTree[T]) {
    def postorder: List[T] = tree.children.flatMap(_.postorder) :+ tree.value
  }

  println("Problem P72")
  println("____________")

  println("string2MTree(\"afg^^c^bd^e^^^\").postorder = " +
           string2MTree("afg^^c^bd^e^^^").postorder)
}
