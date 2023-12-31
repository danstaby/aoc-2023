package aoc

object Point {
  val Up = Point(0, -1)
  val Down = Point(0, 1)
  val Left = Point(-1, 0)
  val Right = Point(1, 0)

  val origo = Point(0, 0)
}

case class Point(x: Long, y: Long) {

  def +(other: Point): Point = Point(x + other.x, y + other.y)
  def -(other: Point): Point = Point(x - other.x, y - other.y)

  def *(factor: Long): Point = Point(x * factor, y * factor)

  def l1Norm(other: Point): Long = Math.abs(x - other.x) + Math.abs(y - other.y)

  def isOrthogonalToOrigo: Boolean = x == 0 || y == 0

  def isAdjacentTo(other: Point): Boolean = {
    val xDiff = Math.abs(x - other.x)
    val yDiff = Math.abs(y - other.y)
    (xDiff == 1 && yDiff == 0) || (xDiff == 0 && yDiff == 1)
  }

  def isAdjacentToDiagonal(other: Point): Boolean = {
    val xDiff = Math.abs(x - other.x)
    val yDiff = Math.abs(y - other.y)
    (xDiff == 1 && yDiff == 1) || isAdjacentTo(other)
  }

  lazy val left: Point = Point(x - 1, y)
  lazy val right: Point = Point(x + 1, y)
  lazy val up: Point = Point(x, y - 1)
  lazy val down: Point = Point(x, y + 1)

  lazy val adjacent: Seq[Point] = Seq(up, down, left, right)

  def allDirections: Seq[Point] = {
    for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } yield Point(this.x + x, this.y + y)

  }
}
