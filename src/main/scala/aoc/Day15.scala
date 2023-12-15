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
    val slots = IndexedSeq.fill(256)(mutable.ListBuffer.empty[(String, Int)])

    parseInput(input).foreach {
      case s"$a=$b" =>
        val m = slots(hash(a))
        if (m.exists(_._1 == a)) m.update(m.indexWhere(_._1 == a), a -> b.toInt)
        else m.append(a -> b.toInt)
      case s"${a}-" =>
        val m = slots(hash(a))
        if (m.exists(_._1 == a)) m.remove(m.indexWhere(_._1 == a))
    }

    slots.zipWithIndex.map { case (box, boxIdx) =>
      (boxIdx.toLong + 1L) * box.zipWithIndex.map { case ((_, focalLength), slotIdx) =>
        (slotIdx + 1) * focalLength.toLong
      }.sum
    }.sum
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
