package aoc

import scala.math.max

object Day2 extends App with Common {

  def parseInput(input: Seq[String]): Seq[Seq[(String, Int)]] = input.map {
    case s"Game $_: $cards" =>
      cards
        .split("[;,] ")
        .map { case s"$count $color" => color -> count.toInt }
  }

  def getMaxCubes(game: Seq[(String, Int)]): Map[String, Int] = game.groupMapReduce(_._1)(_._2)(max)

  def partOne(input: Seq[String]): Int = {
    val maxCubes = Map("red" -> 12, "green" -> 13, "blue" -> 14)

    parseInput(input).zipWithIndex.collect {
      case (game, id) if getMaxCubes(game).forall { case (cr, ct) => ct <= maxCubes(cr) } => id + 1
    }.sum
  }

  def partTwo(input: Seq[String]): Int = parseInput(input).map { game =>
    getMaxCubes(game).values.product
  }.sum

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
