package aoc

object Day8 extends App with Common {

  def parseInput(input: Seq[String]): (Seq[Char], Map[String, (String, String)]) = {
    (input.head.toList, input.drop(2).map { case s"$a = ($b, $c)" => (a, (b, c)) }.toMap)
  }

  def findCycleLength(
      instructions: Seq[Char],
      graph: Map[String, (String, String)],
      start: String
  ): Int = {
    LazyList
      .unfold(0)(i => Some(instructions(i % instructions.length), i + 1))
      .scanLeft(start) { case (acc, dir) =>
        val (left, right) = graph(acc)
        dir match {
          case 'L' => left
          case 'R' => right
        }
      }
      .takeWhile(_.last != 'Z')
      .length
  }

  def partOne(input: Seq[String]): Int = {
    val (instructions, graph) = parseInput(input)

    findCycleLength(instructions, graph, "AAA")
  }

  def partTwo(input: Seq[String]): Long = {
    val (instructions, graph) = parseInput(input)

    graph.keys
      .filter(_.last == 'A')
      .toList
      .map(findCycleLength(instructions, graph, _).toLong / instructions.length)
      .product * instructions.length
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
