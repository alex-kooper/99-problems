import multiwaytree.MTree

object P70 extends App {

  implicit def string2MTree(s: String): MTree[Char] = parseTree(s.toList)._1

  private[this] def parseTree(l: List[Char]): (MTree[Char], List[Char]) = l match {
    case c :: restChars =>
      val (children, charsToPass) = parseChildren(restChars)
      (MTree(c, children), charsToPass)

    case _ => throw new Exception(s"Error: Not a parsable tree")
  }

  private[this] def parseChildren(l: List[Char]): (List[MTree[Char]], List[Char]) = l match {
    case '^' :: rest => (Nil, rest)

    case chars =>
      val (tree, restChars) = parseTree(chars)
      val (children, charsToPass) = parseChildren(restChars)
      (tree :: children, charsToPass)
  }

  println("Problem P70")
  println("____________")

  val s = "afg^^c^bd^e^^^"
  val tree: MTree[Char] = s

  println(s"The original node string: $s")
  println(s"Resulting tree: $tree")
  println(s"Converted back to a node string: ${tree.nodeString}")
}
