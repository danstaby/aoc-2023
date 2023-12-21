package aoc

import scala.annotation.tailrec

object Day21 extends App with Common {

  sealed trait Element
  case object Start extends Element
  case object Garden extends Element
  case object Rock extends Element

  def parseInput(input: Seq[String]): Map[Point, Element] = input.zipWithIndex.flatMap {
    case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        char match {
          case '.' => Point(x, y) -> Garden
          case '#' => Point(x, y) -> Rock
          case 'S' => Point(x, y) -> Start
        }
      }
  }.toMap

  @tailrec
  def findMaxNeighbors(current: Set[Point], map: Map[Point, Element], maxStep: Int): Set[Point] = {
    if (maxStep == 0) current
    else {
      val neighbors = current.flatMap(_.adjacent).filter(map.contains)
      findMaxNeighbors(neighbors, map, maxStep - 1)
    }
  }

  @tailrec
  def findMaxNeighborsInf(
      current: Set[Point],
      previous: Set[Point],
      map: Map[Point, Element],
      maxX: Long,
      maxY: Long,
      maxStep: Int,
      acc: List[Long] = List(1)
  ): List[Long] = {
    if (maxStep == 0) acc
    else {
      val neighbors = current
        .flatMap(_.adjacent)
        .diff(previous)
        .filter(p =>
          map.contains(Point((maxX * 10000000 + p.x) % maxX, (maxY * 1000000 + p.y) % maxY))
        )

      findMaxNeighborsInf(neighbors, current, map, maxX, maxY, maxStep - 1, neighbors.size :: acc)
    }
  }

  def partOne(input: Seq[String]): Int = {
    val map = parseInput(input).filterNot(_._2 == Rock)
    val start = map.find(_._2 == Start).get._1

    findMaxNeighbors(Set(start), map, 64).size
  }
  def partTwo(input: Seq[String], numSteps: Long = 26501365): Long = {
    val map = parseInput(input).filterNot(_._2 == Rock)
    val start = map.find(_._2 == Start).get._1

    val maxX = map.keys.maxBy(_.x).x + 1
    val maxY = map.keys.maxBy(_.y).y + 1

    val width = maxY
    val offset = width / 2

    val visited = findMaxNeighborsInf(Set(start), Set.empty, map, maxX, maxY, 500)

    val cumsums =
      visited.tails.map(_.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).sum).toList.reverse

    val quadratic =
      cumsums.zipWithIndex.collect { case (s, i) if i % width == (numSteps % width) + 1 => s }

    val c = quadratic.head
    val b = (-3 * quadratic.head + 4 * quadratic(1) - quadratic(2)) / 2
    val a = (quadratic.head - 2 * quadratic(1) + quadratic(2)) / 2

    val x = (numSteps - offset) / width

    a * x * x + b * x + c
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))

}
