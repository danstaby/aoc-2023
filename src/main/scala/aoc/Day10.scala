package aoc

import scala.annotation.tailrec

object Day10 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, Char] = input.zipWithIndex.flatMap {
    case (row, y) => row.zipWithIndex.map { case (char, x) => Point(x, y) -> char }
  }.toMap

  def getAdjacent(graph: Map[Point, Char]): Map[Point, Seq[Point]] = graph.map { case (n, c) =>
    c match {
      case '|' => n -> Seq(Point(n.x, n.y - 1), Point(n.x, n.y + 1))
      case '-' => n -> Seq(Point(n.x - 1, n.y), Point(n.x + 1, n.y))
      case 'L' => n -> Seq(Point(n.x + 1, n.y), Point(n.x, n.y - 1))
      case 'J' => n -> Seq(Point(n.x - 1, n.y), Point(n.x, n.y - 1))
      case '7' => n -> Seq(Point(n.x - 1, n.y), Point(n.x, n.y + 1))
      case 'F' => n -> Seq(Point(n.x + 1, n.y), Point(n.x, n.y + 1))
      case '.' => n -> Seq.empty[Point]
      case 'S' =>
        n -> Seq(Point(n.x, n.y - 1), Point(n.x, n.y + 1), Point(n.x - 1, n.y), Point(n.x + 1, n.y))
    }
  }

  @tailrec
  def getMaxSteps(
      graph: Map[Point, Seq[Point]],
      current: List[Point],
      visited: Map[Point, Int],
      steps: Int
  ): Map[Point, Int] = {
    val next = current
      .map(c => (c, graph(c)))
      .flatMap { case (c, ns) => ns.filter(n => graph.contains(n) && graph(n).contains(c)) }
      .filterNot(visited.contains)
      .distinct

    if (next.isEmpty) visited
    else getMaxSteps(graph, next, visited ++ next.map(_ -> (steps + 1)), steps + 1)
  }

  def partOne(input: Seq[String]): Int = {
    val nodes = parseInput(input)
    val graph = getAdjacent(nodes)

    val start = nodes.find(_._2 == 'S').get._1
    getMaxSteps(graph, List(start), Map(start -> 0), 0).values.max
  }

  def partTwo(input: Seq[String]): Int = {
    val nodes = parseInput(input)
    val graph = getAdjacent(nodes)
    val graphWithoutStart = graph.updated(nodes.find(_._2 == 'S').get._1, Seq.empty)

    val start = nodes.find(_._2 == 'S').get._1
    val pathWithSteps = getMaxSteps(graph, List(start), Map(start -> 0), 0)

    @tailrec
    def getPath(current: Point, graph: Map[Point, Seq[Point]], path: List[Point]): List[Point] = {
      val next = graph(current).filterNot(path.contains)
      if (next.isEmpty) path
      else getPath(next.head, graph, next.head :: path)
    }

    val maxNode = pathWithSteps.maxBy(_._2)._1
    val subpaths =
      graphWithoutStart(maxNode).map(n => getPath(n, graphWithoutStart, n :: maxNode :: Nil))

    val nodesInPath = subpaths.head ::: subpaths.last.reverse.tail

    //Calculate if nodes in path is clockwise or anti-clockwise by summing the cross product of
    //the deltas.
    val direction = nodesInPath
      .sliding(3)
      .map { case Seq(a, b, c) => (c - b, b - a) }
      .filter { case (a, b) => a != b }
      .map { case (a, b) => -b.y * a.x + b.x * a.y }
      .sum

    val clockwisePath = if (direction < 0) nodesInPath else nodesInPath.reverse

    val nodesInside =
      clockwisePath
        .sliding(3)
        .foldLeft(List.empty[Point]) { case (acc, Seq(a, b, c)) =>
          Point(b.x + b.y - a.y, b.y + a.x - b.x) :: Point(b.x + c.y - b.y, b.y + b.x - c.x) :: acc
        }
        .distinct
        .filterNot(clockwisePath.contains)

    @tailrec
    def bucketFill(nodes: Set[Point], visited: Set[Point]): Set[Point] = {
      val next = nodes
        .flatMap(_.adjacent.filterNot(n => visited.contains(n) || nodesInPath.contains(n)))
      if (next.isEmpty) visited
      else bucketFill(next, visited ++ next)
    }

    bucketFill(nodesInside.toSet, nodesInside.toSet).size
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
