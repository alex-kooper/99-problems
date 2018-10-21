package fillingshape.mutable

class Field(val sizeX: Int, val sizeY: Int) {
  private val field = Array.fill(sizeX, sizeY)(false)

  def hasPoint(x: Int, y: Int): Boolean = field(x)(y)

  def setPoint(x: Int, y: Int): Unit = field(x)(y) = true
  def removePoint(x: Int, y: Int): Unit = field(x)(y) = false

  override def toString: String = {
    (0 until sizeY).map { y =>
      (0 until sizeX).map { x =>
        if (hasPoint(x, y)) '*' else ' '
      }.mkString
    }.mkString("\n")
  }
}

object Field {
  def fromString(s: String): Field = {
    val points = for {
      (l, y) <- s.split('\n').zip(Stream.from(0))
      (c, x) <- l.zip(Stream.from(0))
      if c == '*'
    } yield (x, y)

    val sizeX = points.map(_._1).max + 1
    val sizeY = points.map(_._2).max + 1

    val field = new Field(sizeX, sizeY)

    for((x, y) <- points)
      field.setPoint(x, y)

    field
  }
}

object FillingShape extends App {
  def adjacentPoints(field: Field, x: Int, y: Int): Seq[(Int, Int)] = {
    for {
      (deltaX, deltaY) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      newX = x + deltaX
      newY = y + deltaY

      if
        (0 until field.sizeX).contains(newX) &&
        (0 until field.sizeY).contains(newY) &&
        !field.hasPoint(newX, newY)
    } yield (newX, newY)
  }

  def dfsFillShape(field: Field, x: Int, y: Int): Unit = {
    if (!field.hasPoint(x, y)) {
      field.setPoint(x, y)

      for ((x, y) <- adjacentPoints(field, x, y))
        dfsFillShape(field, x, y)
    }
  }

  val fieldString =
    """
      |    *************
      |   *           *
      |  *           *
      |   *           *
      |    *           *
      |   *           *
      |  *           *
      |   *************
      |
      |
      |""".stripMargin

  val field = Field.fromString(fieldString)
  val xInside = field.sizeX / 2
  val yInside = field.sizeY / 2

  println("Original Shape")
  println(field)
  println()

  println("DFS Filled Shape")
  dfsFillShape(field, xInside, yInside)
  println(field)
}