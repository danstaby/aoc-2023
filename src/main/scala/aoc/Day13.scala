package aoc

object Day13 extends App with Common {

  def parseInput(input: Seq[String]): Seq[Seq[Point]] = input
    .mkString("\n")
    .split("\n\n")
    .map {
      _.split("\n").toSeq.zipWithIndex.flatMap { case (row, y) =>
        row.zipWithIndex.collect { case (c, x) if c == '#' => Point(x, y) }
      }
    }
    .toSeq

  def pointsInRow(pattern: Seq[Point]): Map[Set[Long], Seq[Long]] = pattern
    .groupBy(_.y)
    .map { case (y, points) => (y, points.map(_.x).toSet) }
    .groupBy(_._2)
    .map { case (k, v) => (k, v.keys.toSeq) }

  def pointsInColumn(pattern: Seq[Point]): Map[Set[Long], Seq[Long]] = pattern
    .groupBy(_.x)
    .map { case (x, points) => (x, points.map(_.y).toSet) }
    .groupBy(_._2)
    .map { case (k, v) => (k, v.keys.toSeq) }

  def findRelection(points: Map[Set[Long], Seq[Long]]): Set[Int] = {
    val centers = points
      .map { case (_, ids) =>
        ids.sorted
          .sliding(2)
          .filter(_.length == 2)
          .filter { case Seq(a, b) => b - a == 1 }
          .map(_.head.toInt)
          .toSeq
      }
      .filter(_.nonEmpty)
      .flatten
      .toSeq

    val ids = points.values.toSeq
    val maxId = ids.map(_.max).max.toInt

    centers.map { c =>
      val isReflection = for {
        i <- 0 until math.min(maxId - c, c + 1)
        isReflection = ids.exists(r => r.contains(c - i) && r.contains(c + i + 1))
      } yield i -> isReflection

      if (isReflection.map(_._2).forall(identity)) c + 1 else 0
    }.toSet - 0
  }

  def findReflectionsSets(pattern: Seq[Point]): (Set[Int], Set[Int]) = {
    val rows = pointsInRow(pattern)
    val cols = pointsInColumn(pattern)

    findRelection(rows) -> findRelection(cols)
  }

  def findReflections(pattern: Seq[Point]): Long = {
    val (rows, cols) = findReflectionsSets(pattern)
    cols.sum + 100L * rows.sum
  }

  def partOne(input: Seq[String]): Long = {
    val patterns = parseInput(input)

    patterns.map(findReflections).sum
  }
  def partTwo(input: Seq[String]): Long = {
    val patterns = parseInput(input)

    patterns.map { ps =>
      val (oldRows, oldCols) = findReflectionsSets(ps)
      val newPatterns = ps.map(ps.toSet - _) ++ (for {
        x <- 0 to ps.map(_.x).max.toInt
        y <- 0 to ps.map(_.y).max.toInt
        if !ps.exists(p => p.x == x && p.y == y)
      } yield ps.toSet + Point(x, y))

      val possible = newPatterns.map(_.toSeq).map(findReflectionsSets)

      val possibleCols = possible.flatMap(_._2).toSet -- oldCols
      val possibleRows = possible.flatMap(_._1).toSet -- oldRows

      possibleCols.sum + 100L * possibleRows.sum
    }.sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
