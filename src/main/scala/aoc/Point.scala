package aoc

object Point {
  def Up = Point(0, -1)
  def Down = Point(0, 1)
  def Left = Point(-1, 0)
  def Right = Point(1, 0)
}

case class Point(x: Int, y: Int) {

  def +(other: Point): Point = Point(x + other.x, y + other.y)

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

  def left: Point = Point(x - 1, y)
  def right: Point = Point(x + 1, y)
  def up: Point = Point(x, y - 1)
  def down: Point = Point(x, y + 1)

  def allDirections: Seq[Point] = {
    for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } yield Point(this.x + x, this.y + y)

  }
}
