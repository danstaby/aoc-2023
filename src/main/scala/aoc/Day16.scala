package aoc

import scala.annotation.tailrec

object Day16 extends App with Common {

  def parseInput(input: Seq[String]): Map[Point, Char] = input.zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map { case (char, x) => Point(x, y) -> char }
  }.toMap

  @tailrec
  def simulateLight(
      points: Set[(Point, Point)],
      visited: Set[(Point, Point)],
      grid: Map[Point, Char]
  ): Set[(Point, Point)] = {
    val newPoints = points
      .filter { case (p, d) => !visited((p + d) -> d) && grid.contains(p + d) }
      .flatMap { case (p, d) =>
        val newDir = (grid(p + d), d) match {
          case ('.', _)                                => List(d)
          case ('|', Point.Up | Point.Down)            => List(d)
          case ('-', Point.Right | Point.Left)         => List(d)
          case ('/', Point.Right) | ('\\', Point.Left) => List(Point.Up)
          case ('/', Point.Up) | ('\\', Point.Down)    => List(Point.Right)
          case ('/', Point.Left) | ('\\', Point.Right) => List(Point.Down)
          case ('/', Point.Down) | ('\\', Point.Up)    => List(Point.Left)
          case ('|', Point.Right) | ('|', Point.Left)  => List(Point.Up, Point.Down)
          case ('-', Point.Up) | ('-', Point.Down)     => List(Point.Left, Point.Right)
          case _                                       => throw new Exception("Invalid input")
        }

        newDir.map(p + d -> (d, _))
      }

    if (newPoints.isEmpty) visited
    else
      simulateLight(
        newPoints.map { case (p, (_, nd)) => p -> nd },
        visited ++ newPoints.map { case (p, (od, _)) => p -> od },
        grid
      )
  }

  def partOne(input: Seq[String]): Int = {
    val grid = parseInput(input)
    val origin = Point(-1, 0) -> Point.Right
    simulateLight(Set(origin), Set.empty, grid).map(_._1).size
  }

  def partTwo(input: Seq[String]): Int = {
    val grid = parseInput(input)

    val startingPoints =
      0L.to(grid.keys.map(_.x).max).map(x => Point(x, -1) -> Point.Down) ++
        0L.to(grid.keys.map(_.x).max).map(x => Point(x, grid.keys.map(_.y).max + 1) -> Point.Up) ++
        0L.to(grid.keys.map(_.y).max).map(y => Point(-1, y) -> Point.Right) ++
        0L.to(grid.keys.map(_.y).max).map(y => Point(grid.keys.map(_.x).max + 1, y) -> Point.Left)

    startingPoints.map { origin =>
      val visited = simulateLight(Set(origin), Set.empty, grid)
      visited.map(_._1).size
    }.max
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
