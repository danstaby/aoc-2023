package aoc

object Day9 extends App with Common {

  def parseInput(input: Seq[String]): Seq[Seq[Int]] =
    input.map { _.split(" ").map(_.toInt).toSeq }

  def predictNext(data: Seq[Int]): Int = {
    val diffs = data.sliding(2).map { case Seq(a, b) => b - a }.toList

    if (diffs.forall(_ == 0)) data.last
    else data.last + predictNext(diffs)
  }

  def predictPrev(data: Seq[Int]): Int = predictNext(data.reverse)

  def partOne(input: Seq[String]): Int = {
    parseInput(input).map(predictNext).sum

  }
  def partTwo(input: Seq[String]): Int = {
    parseInput(input).map(predictPrev).sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
