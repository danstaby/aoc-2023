package aoc

import scala.math.max

object Day2 extends App with Common {

  def parseInput(input: Seq[String]): Seq[Seq[Map[String, Int]]] = input.map {
    case s"Game $_: $cards" =>
      cards
        .split("; ")
        .toSeq
        .map(_.split(", ").map { case s"$count $color" => color -> count.toInt }.toSeq.toMap)
  }

  def partOne(input: Seq[String]): Int = {

    val maxCubes = Map("red" -> 12, "green" -> 13, "blue" -> 14)

    parseInput(input).zipWithIndex.collect {
      case (game, id) if game.flatten.groupMapReduce(_._1)(_._2)(max).toSeq.toMap.forall {
            case (color, count) => count <= maxCubes(color)
          } =>
        id + 1
    }.sum
  }
  def partTwo(input: Seq[String]): Int = parseInput(input).map { game =>
    game.flatten.groupMapReduce(_._1)(_._2)(max).values.product
  }.sum

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
