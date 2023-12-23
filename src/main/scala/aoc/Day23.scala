package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day23 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, Char] = input.zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map { case (c, x) => Point(x, y) -> c }
  }.toMap

  def longestPath(
      map: Map[Point, Char],
      start: Point,
      end: Point
  ): Int = {
    val queue = mutable.Queue((start, 0, start + Point(0, -1), Set.empty[Point]))

    var maxResult: Int = 0

    while (queue.nonEmpty) {
      val (point, steps, previous, visited) = queue.dequeue()
      if (point == end) maxResult = steps

      val next = point.adjacent
        .filterNot(visited.contains)
        .filter(_ != previous)
        .filter(map.contains)
        .filter { n =>
          map(n) match {
            case '.' => true
            case '^' => n - point == Point.Up
            case '>' => n - point == Point.Right
            case 'v' => n - point == Point.Down
            case '<' => n - point == Point.Left
            case _   => false
          }
        }

      val newVisited = if (next.size > 1) visited + point else visited

      next.foreach { p => queue.enqueue((p, steps + 1, point, newVisited)) }
    }
    maxResult
  }

  def simplifyGraph(
      map: Map[Point, Char],
      start: Point,
      end: Point
  ): Map[Point, List[(Point, Int)]] = {
    val graph = map
      .filter(_._2 != '#')
      .map { case (p, _) =>
        val neighbours = p.adjacent.filter(map.contains).filter { n =>
          map(n) match {
            case '#' => false
            case _   => true
          }
        }

        p -> neighbours.map(n => (n, 1, p)).toList
      }

    val edges = graph.filter { case (p, neighbours) =>
      neighbours.size >= 3 || p == start || p == end
    }

    @tailrec
    def reduceGraph(
        edges: Map[Point, List[(Point, Int, Point)]]
    ): Map[Point, List[(Point, Int)]] = {
      val newEdges = edges.map { case (p, neighbors) =>
        p -> neighbors.map { case (n, d, prev) =>
          val next = graph(n).filter(_._1 != prev)

          if (next.size == 1) (next.head._1, d + 1, n)
          else if (next.isEmpty) (n, d, prev)
          else (n, d, n)
        }
      }

      if (newEdges == edges) edges.map { case (p, ns) =>
        p -> ns.map { case (n, d, _) => n -> d }
      }
      else reduceGraph(newEdges)
    }

    reduceGraph(edges)
  }

  def longestPathDfs(
      current: Point,
      end: Point,
      graph: Map[Point, List[(Point, Int)]],
      visited: Set[Point]
  ): Int = {
    val next = graph(current).filterNot { case (p, _) => visited.contains(p) }
    if (current == end) 0
    else if (next.isEmpty) Int.MinValue
    else next.map { case (p, d) => d + longestPathDfs(p, end, graph, visited + p) }.max
  }

  def partOne(input: Seq[String]): Int = {
    val map = parseInput(input)

    val start = map.find { case (p, c) => c == '.' && p.y == 0 }.get._1
    val end = map.find { case (p, c) => c == '.' && p.y == map.keys.maxBy(_.y).y }.get._1

    longestPath(map, start, end)
  }

  def partTwo(input: Seq[String]): Int = {
    val map = parseInput(input)

    val start = map.find { case (p, c) => c == '.' && p.y == 0 }.get._1
    val end = map.find { case (p, c) => c == '.' && p.y == map.keys.maxBy(_.y).y }.get._1

    val graph = simplifyGraph(map, start, end)
    longestPathDfs(start, end, graph, Set(start))
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
