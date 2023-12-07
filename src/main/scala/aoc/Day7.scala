package aoc

object Day7 extends App with Common {

  lazy val cardValue =
    Seq("Joker", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A").zipWithIndex.toMap

  def parseInput(input: Seq[String]): Seq[(Seq[String], Int)] = input.map { case s"$cards $bid" =>
    (cards.toList.map(_.toString), bid.toInt)
  }

  def scoreHands(hands: Seq[(Seq[Int], Int)]): Int = hands
    .sortWith((a, b) => compareHands(a._1, b._1))
    .reverse
    .zipWithIndex
    .map { case ((_, bid), i) => bid * (i + 1) }
    .sum

  def createHandList(cards: Seq[Int]): Seq[Int] = {
    val counts = cards.filter(_ != 0).groupBy(identity).map(_._2.size).toSeq.sorted.reverse
    (counts.headOption.map(_ + 5 - counts.sum).orElse(Option(5)) ++ counts.drop(1)).toSeq
      .padTo(5, 0) ++ cards
  }

  def compareHands(a: Seq[Int], b: Seq[Int]): Boolean =
    createHandList(a)
      .zip(createHandList(b))
      .collectFirst { case (a, b) if a != b => a > b }
      .getOrElse(false)

  def partOne(input: Seq[String]): Int =
    scoreHands(parseInput(input).map { case (card, bid) => (card.map(cardValue), bid) })

  def partTwo(input: Seq[String]): Int = {
    val jokerInput = parseInput(input).map { case (cards, bid) =>
      (cards.map(c => if (c == "J") "Joker" else c), bid)
    }

    scoreHands(jokerInput.map { case (card, bid) => (card.map(cardValue), bid) })
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
