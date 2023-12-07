package aoc

object Day7 extends App with Common {

  def parseInput(input: Seq[String]): Seq[(Seq[String], Int)] = input.map { case s"$cards $bid" =>
    (cards.toList.map(_.toString), bid.toInt)
  }
  def getHandValue(cardsCounts: Seq[Int]): Int = {
    cardsCounts match {
      case Seq(5)             => 7
      case Seq(1, 4)          => 6
      case Seq(2, 3)          => 5
      case Seq(1, 1, 3)       => 4
      case Seq(1, 2, 2)       => 3
      case Seq(1, 1, 1, 2)    => 2
      case Seq(1, 1, 1, 1, 1) => 1
    }
  }

  def countCards(cards: Seq[String]): Seq[Int] =
    cards.groupBy(identity).map { case (_, v) => v.size }.toSeq.sorted

  def getHandType(cards: Seq[String]): Int = {
    getHandValue(countCards(cards))
  }

  def getHandTypeJoker(cards: Seq[String]): Int = {
    var handGrouped = countCards(cards.filter(_ != "J")).toArray

    if (handGrouped.isEmpty) handGrouped = Array(5)
    else
      handGrouped(handGrouped.length - 1) =
        (5 - handGrouped.sum) + handGrouped(handGrouped.length - 1)

    getHandValue(handGrouped)
  }

  def compareHands(
      a: Seq[String],
      b: Seq[String],
      cardValue: Map[String, Int],
      t: Seq[String] => Int
  ): Boolean = {
    val aType = t(a)
    val bType = t(b)

    if (aType != bType) aType > bType
    else {
      a.map(cardValue).zip(b.map(cardValue)).find { case (a, b) => a != b } match {
        case Some((a, b)) => a > b
        case None         => true
      }
    }
  }

  def scoreHands(
      input: Seq[String],
      cardValue: Map[String, Int],
      handType: Seq[String] => Int
  ): Int =
    parseInput(input)
      .sortWith((a, b) => compareHands(a._1, b._1, cardValue, handType))
      .reverse
      .zipWithIndex
      .map { case ((_, bid), i) => bid * (i + 1) }
      .sum

  def partOne(input: Seq[String]): Int = {
    val cardValue =
      Seq('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
        .map(_.toString)
        .zipWithIndex
        .toMap

    scoreHands(input, cardValue, getHandType)
  }

  def partTwo(input: Seq[String]): Int = {
    val cardValue =
      Seq('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
        .map(_.toString)
        .zipWithIndex
        .toMap

    scoreHands(input, cardValue, getHandTypeJoker)
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
