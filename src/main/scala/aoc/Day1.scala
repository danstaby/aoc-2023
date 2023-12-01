package aoc

object Day1 extends App with Common {

  lazy val digits =
    Seq("NEVERMATCH", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def partOne(input: Seq[String]): Int = input.map { s =>
    val ds = s.filter(c => c.isDigit)
    ds.head.toString.toInt * 10 + ds.last.toString.toInt
  }.sum

  def findDigit(s: String): Option[Int] = {
    if (s.head.isDigit) s.head.toString.toIntOption
    else digits.zipWithIndex.find { case (d, _) => s.startsWith(d) }.map(_._2)
  }

  def partTwo(input: Seq[String]): Int = input.flatMap { s =>
    val ds = s.toList.tails.toList
      .map(_.mkString)
      .filter(_.nonEmpty)
      .filter(s => s.head.isDigit || digits.exists(d => s.startsWith(d)))

    for {
      d1 <- findDigit(ds.head)
      d2 <- findDigit(ds.last)
    } yield d1 * 10 + d2

  }.sum

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
