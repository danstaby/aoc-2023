package aoc

object Day6 extends App with Common {

  def parseInput(input: Seq[String]): Seq[Seq[Int]] = input.map { case s"$_: $nums" =>
    nums.trim.split("[ ]+").map(_.toInt).toSeq
  }

  def partOne(input: Seq[String]): Int =
    parseInput(input).transpose.map { case Seq(time, distanceMax) =>
      1 to time map (t => t * (time - t)) count (_ > distanceMax)
    }.product

  /*
    The solution to the equation t * (time - t) = distanceMax is:
    t = (time +- sqrt(time^2 - 4 * distanceMax)) / 2

    Find the number if integer times between solution t0 and t1:
   */
  def partTwo(input: Seq[String]): Long =
    parseInput(input).map(_.map(_.toString).reduce(_ + _).toLong) match {
      case Seq(time, distanceMax) =>
        val t0 = (time + math.sqrt(time * time - 4 * (distanceMax + 1))) / 2
        val t1 = (time - math.sqrt(time * time - 4 * (distanceMax + 1))) / 2

        Math.floor(t0).toLong - Math.ceil(t1).toLong + 1
    }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
