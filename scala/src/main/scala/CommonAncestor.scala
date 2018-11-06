import multiwaytree.MTree
import P73.lispyTree2MTree

object CommonAncestor extends App {

  def pathFromNode[T](node: MTree[T], value: T): Option[List[T]] = {
    if(node.value != value) {
      node.children
        .map(pathFromNode(_, value))
        .find(_.isDefined)
        .flatten
        .map(node.value :: _)
    } else Some(List(node.value))
  }

  def commonAncestor[T](tree: MTree[T], value1: T, value2: T): Option[T] = for {
    path1 <- pathFromNode(tree, value1)
    path2 <- pathFromNode(tree, value2)
  } yield path1.zip(path2)
      .takeWhile { case (x, y) => x == y }
      .last
      ._1

  val tree =
    """
      |(abc
      |  (f
      |    g)
      |  ccc
      |  (b
      |    d
      |    e))
    """.stripMargin

  println(commonAncestor(lispyTree2MTree(tree), "d", "e"))
}
