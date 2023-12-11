package aoc

object Day11 extends App with Common {

  def parseInput(input: Seq[String]): Set[Point] =
    input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (c, x) => if (c == '#') Some(Point(x, y)) else None }
    }.toSet

  def calculateSummedDistances(input: Seq[String], factor: Int): Long = {
    val data = parseInput(input)

    val emptyCols = 0L.to(data.map(_.x).max).filterNot(data.map(_.x).contains)
    val emptyRows = 0L.to(data.map(_.y).max).filterNot(data.map(_.y).contains)

    val stars = data.map { p =>
      Point(p.x + factor * emptyCols.count(_ < p.x), p.y + factor * emptyRows.count(_ < p.y))
    }.toSeq

    val distances = for { star1 <- stars; star2 <- stars } yield star1.l1Norm(star2)

    distances.sum / 2
  }

  def partOne(input: Seq[String]): Long = calculateSummedDistances(input, 1)
  def partTwo(input: Seq[String]): Long = calculateSummedDistances(input, 1000000 - 1)

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
