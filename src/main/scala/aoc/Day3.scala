package aoc

import scala.annotation.tailrec

object Day3 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, String] = input.zipWithIndex
    .flatMap { case (line, y) =>
      line.zipWithIndex.map { case (char, x) => Point(x, y) -> char.toString }
    }
    .filter(_._2 != ".")
    .toMap

  def getNumbers(ps: Map[Point, String]): List[List[Point]] = {
    @tailrec
    def rec(ps: Map[Point, String], p: Point, l: List[List[Point]]): List[List[Point]] =
      l match {
        case Nil :: t if ps.contains(p.left) => rec(ps, p.left, Nil :: t)
        case h :: t if ps.contains(p.right)  => rec(ps.removed(p), p.right, (p :: h) :: t)
        case h :: t if ps.size >= 2          => rec(ps - p, (ps - p).head._1, Nil :: (p :: h) :: t)
        case h :: t                          => (p :: h) :: t
      }

    val psOnlyNumbers = ps.filter(_._2.matches("[0-9]"))
    rec(psOnlyNumbers, psOnlyNumbers.head._1, Nil :: Nil)
  }

  def partOne(input: Seq[String]): Int = {
    val ps = parseInput(input)

    val symbols = ps.filter(!_._2.matches("[0-9]"))

    getNumbers(ps)
      .filter(_.exists(_.allDirections.exists(symbols.contains)))
      .map { _.sortBy(_.x).map(ps).reduce(_ + _).toInt }
      .sum
  }

  def partTwo(input: Seq[String]): Int = {
    val ps = parseInput(input)
    val numbers = getNumbers(ps)

    ps.filter(!_._2.matches("[0-9]"))
      .map { case (p, _) => numbers.filter(_.exists(_.isAdjacentToDiagonal(p))) }
      .filter(_.size == 2)
      .map { ns =>
        ns.map(points => points.toSeq.sortBy(_.x).map(ps(_)).reduce(_ + _).toInt).product
      }
      .sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
