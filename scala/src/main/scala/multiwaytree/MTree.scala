package multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())

    /**
      * @return Return a string with a scala code that if executed would create this tree
      */
    def toMTreeString: String = {
      if (children.isEmpty)
        s"MTree('$value')"
      else
        s"MTree('$value', List(${children.map(_.toMTreeString).mkString(", ")}))"
    }

    override def toString: String = s"$value${children.map(_.toString).mkString("")}^"
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  }
}