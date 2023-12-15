package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 extends App with Common {

  def parseInput(input: Seq[String]): Seq[String] = input.head.split(",")

  @tailrec
  def hash(s: String, acc: Int = 0): Int = {
    if (s.nonEmpty) hash(s.tail, (s.head.toInt + acc) * 17 % 256)
    else acc
  }

  def partOne(input: Seq[String]): Long = parseInput(input).map(s => hash(s).toLong).sum

  def partTwo(input: Seq[String]): Long = {
    val slots = IndexedSeq.fill(256)(mutable.LinkedHashMap.empty[String, Int])

    parseInput(input).foreach {
      case s"$a=$b" => slots(hash(a)).update(a, b.toInt)
      case s"${a}-" => slots(hash(a)).remove(a)
    }

    slots.zipWithIndex.map { case (box, boxIdx) =>
      (boxIdx + 1) * box.zipWithIndex.map { case ((_, l), i) => (i + 1) * l }.sum
    }.sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
