package aoc

object Day4 extends App with Common {

  def parseCard(input: String): List[Int] = input.trim.split("[ ]+").map(_.toInt).toList

  def parseInput(input: Seq[String]): Seq[(List[Int], List[Int])] = input.map {
    case s"Card $_: $winning | $mine" => (parseCard(winning), parseCard(mine))
  }

  def calculateScore(winning: List[Int], mine: List[Int]): Int = {
    val matches = mine.count(winning.contains)
    if (matches == 0) 0 else math.pow(2, matches - 1).toInt
  }

  def partOne(input: Seq[String]): Int =
    parseInput(input).map { case (winning, mine) => calculateScore(winning, mine) }.sum

  def partTwo(input: Seq[String]): Int = {
    val cards = parseInput(input).toList

    val cardScore = Array.fill(cards.length)(0)

    for { i <- cards.length - 1 to 0 by -1 } {
      val (winning, mine) = cards(i)
      val score = mine.count(winning.contains)
      cardScore(i) = 1 + (1 to score).map(s => cardScore(i + s)).sum
    }

    cardScore.sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
