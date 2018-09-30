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

  def lispyTree2MTree(s: String): MTree[String] = parseTree(tokens(s.toStream))._1

  type Token = String

  def tokens(chars: Stream[Char]): Stream[Token] = chars match {
    case Stream.Empty => Stream.empty
    case '(' #:: rest => "(" #:: tokens(rest)
    case ')' #:: rest => ")" #:: tokens(rest)
    case c #:: rest if c.isWhitespace => tokens(rest)
    case cs =>
      val (token, restTokens) = cs.span(c => !c.isWhitespace && !"()".contains(c))
      token.mkString #:: tokens(restTokens)
  }

  private[this] def parseTree(tokens: Stream[Token]): (MTree[String], Stream[Token]) = tokens match {
    case "(" #:: token #:: restTokens =>
      val (children, notParsedTokens)  = parseChildren(restTokens)
      (MTree(token, children), notParsedTokens)

    case token #:: restTokens =>
      (MTree(token), restTokens)

    case _ => throw new Exception("Parsing error: Not a well formed lispy tree")
  }

  private[this] def parseChildren(tokens: Stream[Token]): (List[MTree[String]], Stream[Token]) = tokens match {
    case ")" #:: restTokens => (List.empty, restTokens)

    case tokens =>
      val (tree, notParsedTokens) = parseTree(tokens)
      val (children, tokensToPass) = parseChildren(notParsedTokens)
      (tree :: children, tokensToPass)
  }

  println("Problem P73")
  println("____________")

  val lispyTree =
    """
      |(abc
      |  (f
      |    g)
      |  ccc
      |  (b
      |    d
      |    e))
    """.stripMargin

  val tree = lispyTree2MTree(lispyTree)

  println(s"The original lispy tree: $lispyTree")
  println(s"It's internal representation: $tree")
  println(s"Converted back to lispy tree: ${tree.listpyTree}")
}
