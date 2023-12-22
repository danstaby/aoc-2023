package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day22 extends App with Common {

  case class Point3D(x: Long, y: Long, z: Long) {
    def +(other: Point3D): Point3D = Point3D(x + other.x, y + other.y, z + other.z)
    def -(other: Point3D): Point3D = Point3D(x - other.x, y - other.y, z - other.z)
  }

  def parseInput(input: Seq[String]): Seq[(Point3D, Point3D)] = input
    .map { case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
      (Point3D(x1.toLong, y1.toLong, z1.toLong), Point3D(x2.toLong, y2.toLong, z2.toLong))
    }

  @tailrec
  def simulateFalling(blocks: Seq[Seq[Point3D]]): Seq[Seq[Point3D]] = {

    val newSettled = mutable.HashSet.empty[Point3D]
    val sortedBlocks = blocks.sortBy(_.map(_.z).min)

    sortedBlocks.foreach { block =>
      if (block.exists(_.z == 1) || block.map(_ + Point3D(0, 0, -1)).exists(newSettled.contains)) {
        newSettled ++= block
      }
    }

    val newBlocks = sortedBlocks.map { block =>
      if (block.exists(newSettled.contains)) block
      else if (block.map(_ + Point3D(0, 0, -1)).exists(newSettled.contains)) block
      else block.map(_ + Point3D(0, 0, -1))
    }

    if (blocks == newBlocks) newBlocks else simulateFalling(newBlocks)
  }

  def calculateGraph(input: Seq[String]): Map[Int, Seq[Int]] = {
    val bricks = parseInput(input)

    val points = bricks.map {
      case (p1, p2) => {
        for { x <- p1.x to p2.x; y <- p1.y to p2.y; z <- p1.z to p2.z } yield Point3D(x, y, z)
      }
    }

    val endState = simulateFalling(points)

    endState.zipWithIndex.map { case (block, i) =>
      val other = endState.zipWithIndex.filter(_._2 != i)
      val supported = other.filter { case (ps, j) =>
        ps.map(_ + Point3D(0, 0, 1)).exists(block.contains)
      }

      (i, supported.map(_._2))
    }.toMap
  }

  def partOne(input: Seq[String]): Int = {
    val graph = calculateGraph(input)
    val unremovable = graph.filter(_._2.size == 1).flatMap(_._2).toSet

    (graph.keySet -- unremovable).size
  }
  def partTwo(input: Seq[String]): Int = {
    val graph = calculateGraph(input)

    @tailrec
    def countDegree(visited: Set[Int], graph: Map[Int, Seq[Int]]): Int = {
      val next = graph.filter { case (i, deps) =>
        deps.forall(visited.contains) && deps.nonEmpty
      }.keySet -- visited

      if (next.isEmpty) visited.size - 1
      else countDegree(visited ++ next, graph -- next)

    }

    graph.keys.toSeq.map(i => countDegree(Set(i), graph)).sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
