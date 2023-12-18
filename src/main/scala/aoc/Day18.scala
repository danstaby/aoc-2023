package aoc

object Day18 extends App with Common {

  def parseInput(input: Seq[String]): Seq[(Point, Int, String)] = input.map {
    case s"U $steps ($color)" => (Point.Up, steps.toInt, color)
    case s"D $steps ($color)" => (Point.Down, steps.toInt, color)
    case s"L $steps ($color)" => (Point.Left, steps.toInt, color)
    case s"R $steps ($color)" => (Point.Right, steps.toInt, color)
  }

  def polygonArea(vertices: Seq[Point]): Long = vertices
    .sliding(2)
    .map { case Seq(v1, v2) => v1.x * v2.y - v1.y * v2.x }
    .sum
    .abs / 2

  def calculateVertices(data: Seq[(Long, Point)]): Seq[Point] = data.scanLeft(Point.origo) {
    case (p, (steps, direction)) => p + direction * steps
  }

  def partOne(input: Seq[String]): Long = {
    val data = parseInput(input).map { case (direction, steps, _) => steps.toLong -> direction }

    polygonArea(calculateVertices(data)) + data.map(_._1).sum / 2 + 1
  }

  def partTwo(input: Seq[String]): Long = {

    val data = parseInput(input).map {
      case (_, _, s"#${l}0") => (BigInt(l, 16).toLong, Point.Right)
      case (_, _, s"#${l}1") => (BigInt(l, 16).toLong, Point.Down)
      case (_, _, s"#${l}2") => (BigInt(l, 16).toLong, Point.Left)
      case (_, _, s"#${l}3") => (BigInt(l, 16).toLong, Point.Up)
    }

    polygonArea(calculateVertices(data)) + (data.map(_._1).sum / 2) + 1
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
