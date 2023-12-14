package aoc

import scala.collection.mutable.ArrayBuffer

object Day14 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, Char] = input.zipWithIndex
    .flatMap { case (line, y) => line.zipWithIndex.map { case (char, x) => Point(x, y) -> char } }
    .toMap
    .filter(_._2 != '.')

  def rotate(rocks: Set[Point], direction: Point, maxX: Long, maxY: Long): Set[Point] =
    direction match {
      case Point.Up    => rocks
      case Point.Down  => rocks.map(p => Point(p.x, maxY - p.y))
      case Point.Left  => rocks.map(p => Point(maxY - p.y, p.x))
      case Point.Right => rocks.map(p => Point(p.y, maxX - p.x))
    }

  def inverseRotate(rocks: Set[Point], direction: Point, maxX: Long, maxY: Long): Set[Point] = {
    direction match {
      case Point.Up | Point.Down => rotate(rocks, direction, maxX, maxY)
      case _                     => rotate(rocks, direction * -1, maxY, maxX)
    }
  }

  def rollRocks(
      round: Set[Point],
      square: Map[Point, Map[Long, List[Long]]],
      direction: Point,
      maxX: Long,
      maxY: Long
  ): Set[Point] = {

    val roundRocks = rotate(round, direction, maxX, maxY).groupBy(_.x)

    val newRocks = for {
      x <- 0L to square(direction).keys.max
      ys = square(direction).getOrElse(x, Seq.empty)
      Seq(miny, maxy) <- (-1L +: ys :+ Long.MaxValue).sliding(2)
      ps = roundRocks.get(x).toSeq.flatMap(_.filter { p => p.y > miny && p.y < maxy })
      newPs <- 1.to(ps.size).map(i => Point(x, i + miny))
    } yield newPs

    inverseRotate(newRocks.toSet, direction, maxX, maxY)
  }

  def getSquareRocks(square: Set[Point]): Map[Point, Map[Long, List[Long]]] =
    Point(0, 0).adjacent.map { d =>
      val (maxX, maxY) = (square.map(_.x).max, square.map(_.y).max)
      d -> rotate(square, d, maxX, maxY)
        .groupBy(_.x)
        .map { case (k, v) => k -> v.map(_.y).toList.sorted }
    }.toMap

  def partOne(input: Seq[String]): Long = {
    val rocks = parseInput(input)
    val (maxX, maxY) = (rocks.map(_._1.x).max, rocks.map(_._1.y).max)

    val newRound = rollRocks(
      rocks.filter { case (p, c) => c == 'O' }.keys.toSet,
      getSquareRocks(rocks.filter { case (p, c) => c == '#' }.keys.toSet),
      Point.Up,
      maxX,
      maxY
    )

    val values = newRound.map(p => p -> (rocks.keys.map(_.y).max - p.y + 1)).toSeq

    values.map(_._2).sum
  }

  def partTwo(input: Seq[String]): Long = {
    val original = parseInput(input)
    val (maxX, maxY) = (original.map(_._1.x).max, original.map(_._1.y).max)

    val roundRocks = original.filter { case (p, c) => c == 'O' }.keys.toSet
    val square = original.filter { case (p, c) => c == '#' }.keys.toSet
    val squareRocks = getSquareRocks(square)

    val mem = collection.mutable.Map.empty[Set[Point], ArrayBuffer[Int]]

    def cycle(rocks: Set[Point]): Set[Point] = {
      val north = rollRocks(rocks, squareRocks, Point.Up, maxX, maxY)
      val west = rollRocks(north, squareRocks, Point.Left, maxX, maxY)
      val south = rollRocks(west, squareRocks, Point.Down, maxX, maxY)
      rollRocks(south, squareRocks, Point.Right, maxX, maxY)
    }

    var res = roundRocks
    var i = 0

    while (!(mem.contains(res) && mem(res).size == 2)) {
      i += 1
      res = cycle(res)
      mem.update(res, mem.getOrElse(res, ArrayBuffer.empty) :+ i)
    }

    val firstCycle = mem.find(_._2.size == 2).map(_._2).head

    val cycleLength = firstCycle.last - firstCycle.head
    val remaining = (1000000000L - i) % cycleLength

    val endState = mem.find(_._2.head == (i - cycleLength + remaining)).map(_._1).get

    endState.toSeq.map(p => square.map(_.y).max - p.y + 1).sum
  }

  println("Part 1: " + partOne(testInput))
  println("Part 2: " + partTwo(problemInput))
}
