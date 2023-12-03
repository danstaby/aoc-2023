package aoc

import scala.annotation.tailrec

object Day3 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, String] = input.zipWithIndex
    .flatMap { case (line, y) =>
      line.zipWithIndex.map { case (char, x) => Point(x, y) -> char.toString }
    }
    .filter(_._2 != ".")
    .toMap

  @tailrec
  def buildNumbers(
      ps: Map[Point, String],
      p: Point,
      l: List[Set[Point]] = Nil,
      acc: Set[Point] = Set.empty
  ): List[Set[Point]] = {
    val newPs = ps.removed(p)
    if (ps.contains(p.left) && acc.isEmpty) buildNumbers(ps, p.left, l)
    else if (ps.contains(p.right)) buildNumbers(newPs, p.right, l, acc + p)
    else if (newPs.nonEmpty) buildNumbers(newPs, newPs.keys.head, (acc + p) :: l)
    else (acc + p) :: l
  }

  def getNumbers(ps: Map[Point, String]): List[Set[Point]] = {
    val psOnlyNumbers = ps.filter(_._2.matches("[0-9]"))
    buildNumbers(psOnlyNumbers, psOnlyNumbers.keys.head)
  }

  def partOne(input: Seq[String]): Int = {
    val ps = parseInput(input)

    getNumbers(ps)
      .filter(
        _.flatMap(_.allDirections.filter(ps.contains))
          .exists(ps(_).matches("[^0-9]"))
      )
      .map { _.toSeq.sortBy(_.x).map(ps(_)).reduce(_ + _).toInt }
      .sum
  }

  def partTwo(input: Seq[String]): Int = {
    val ps = parseInput(input)

    val numbers = getNumbers(ps)

    val matchingNumbers = ps
      .collect {
        case (p, s) if !s.matches("[0-9]") =>
          numbers.filter(ns => p.allDirections.exists(ns.contains))
      }
      .filter(_.size == 2)

    matchingNumbers.map { ns =>
      ns.map(points => points.toSeq.sortBy(_.x).map(ps(_)).reduce(_ + _).toInt).product
    }.sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
