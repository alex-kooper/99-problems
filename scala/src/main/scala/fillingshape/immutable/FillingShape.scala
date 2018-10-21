package fillingshape.immutable

import scala.annotation.tailrec

case class Point(x: Int, y: Int)

class Field private (val sizeX: Int, val sizeY: Int, points: Set[Point]) {
  def hasPoint(p: Point): Boolean = points.contains(p)

  def setPoint(p: Point): Field = new Field(sizeX, sizeY, points + p)
  def removePoint(p: Point): Field = new Field(sizeX, sizeY, points - p)

  override def toString: String = {
    (0 until sizeY).map { y =>
      (0 until sizeX).map { x =>
        if (hasPoint(Point(x, y))) '*' else ' '
      }.mkString
    }.mkString("\n")
  }
}

object Field {
  def apply(sizeX: Int, sizeY: Int) = new Field(sizeX, sizeY, Set.empty[Point])

  def fromString(s: String): Field = {
    val points = for {
      (l, y) <- s.split('\n').zip(Stream.from(0))
      (c, x) <- l.zip(Stream.from(0))
      if c == '*'
    } yield Point(x, y)

    val sizeX = points.map(_.x).max + 1
    val sizeY = points.map(_.y).max + 1

    points.foldLeft(Field(sizeX, sizeY))(_.setPoint(_))
  }
}


object FillingShape extends App {
  def adjacentPoints(field: Field, point: Point): Seq[Point] = {
    for {
      (deltaX, deltaY) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      newPoint = Point(point.x + deltaX, point.y + deltaY)

      if
        (0 until field.sizeX).contains(newPoint.x) &&
        (0 until field.sizeY).contains(newPoint.y) &&
        !field.hasPoint(newPoint)
    } yield newPoint
  }

  def dfsFillShape(field: Field, pointInside: Point): Field = {
    if (field.hasPoint(pointInside))
      field
    else {
      val newField = field.setPoint(pointInside)
      adjacentPoints(field, pointInside).foldLeft(newField)(dfsFillShape)
    }
  }

  def bfsFillShape(field: Field, pointInside: Point): Field = {
    @tailrec
    def loop(field: Field, points: List[Point]): Field = {
      val nextField = points.foldLeft(field)(_.setPoint(_))
      val nextPoints = points.flatMap(adjacentPoints(field, _))

      if(nextPoints.isEmpty)
        nextField
      else
        loop(nextField, nextPoints)
    }

    loop(field, List(pointInside))
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
  val pointInside = Point(field.sizeX / 2, field.sizeY / 2)

  println("Original Shape")
  println(field)

  println()

  println("DFS Filled Shape")
  println(dfsFillShape(field, pointInside))

  println()

  println("BFS Filled Shape")
  println(bfsFillShape(field, pointInside))
}
