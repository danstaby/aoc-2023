package aoc

object DayTemplate extends App with Common {

  def parseInput(input: Seq[String]): Seq[String] = input

  def partOne(input: Seq[String]): Int = {
    parseInput(input).foreach(println)
    -1
  }
  def partTwo(input: Seq[String]): Int = -1

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
