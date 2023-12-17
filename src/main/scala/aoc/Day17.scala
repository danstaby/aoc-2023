package aoc

import scala.collection.mutable

object Day17 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, Int] = input.zipWithIndex.flatMap {
    case (line, y) =>
      line.zipWithIndex.map { case (char, x) => Point(x, y) -> char.toString.toInt }
  }.toMap

  def minimumDistance(start: Point, graph: Map[Point, Int], min: Int, max: Int): Int = {
    val maxX = graph.map(_._1.x).max
    val maxY = graph.map(_._1.y).max

    val heap = mutable.PriorityQueue.empty[(Int, (Point, Point, Int))](
      Ordering.by[(Int, (Point, Point, Int)), Int](_._1).reverse
    )

    val visited = mutable.HashSet.empty[(Point, Point, Int)]

    heap.enqueue(0 -> (start, start, 0))

    while (heap.nonEmpty) {
      val (value, (current, lastDir, steps)) = heap.dequeue()

      val dirs = (lastDir, steps) match {
        case (Point.origo, _) => Point.Down :: Point.Right :: Nil
        case (d, steps) if min.until(max).contains(steps) =>
          Point.origo.adjacent.filter(_ != d * -1)
        case (d, steps) if steps < min => d :: Nil
        case (d, steps) if steps == max =>
          Point.origo.adjacent.filter(x => x != d * -1 && x != d)
      }

      val next = dirs
        .map(d => (current + d, d, if (d == lastDir) steps + 1 else 1))
        .filter(x => graph.contains(x._1))
        .filterNot(visited.contains)
        .map { case (n, d, dn) => (value + graph(n), (n, d, dn)) }

      if (next.exists { case (_, (n, _, _)) => n == Point(maxX, maxY) })
        return next.find { case (_, (n, _, _)) => n == Point(maxX, maxY) }.get._1
      else {
        visited.addAll(next.map { case (_, x) => x })
        heap.enqueue(next: _*)
      }
    }

    -1
  }

  def partOne(input: Seq[String]): Int =
    minimumDistance(Point(0, 0), parseInput(input), 1, 3)

  def partTwo(input: Seq[String]): Int =
    minimumDistance(Point(0, 0), parseInput(input), 4, 10)

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
