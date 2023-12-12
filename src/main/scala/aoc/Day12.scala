package aoc

import scala.annotation.tailrec

object Day12 extends App with Common {

  sealed trait Condition

  case object Operational extends Condition
  case object Damaged extends Condition
  case object Unknown extends Condition

  def parseInput(input: Seq[String]): Seq[(Seq[Condition], Seq[Int])] = input.map {
    case s"$conditions $directions" =>
      (
        conditions.map {
          case '.' => Operational
          case '#' => Damaged
          case '?' => Unknown
        },
        directions.split(",").map(_.toInt).toSeq
      )
  }

  @tailrec
  def generatePermutations(
      conditions: IndexedSeq[Condition],
      directions: IndexedSeq[Int],
      acc: List[(List[Int], Long)],
      i: Int
  ): List[(Seq[Int], Long)] = {
    if (i == conditions.length) {
      acc
        .map { case (x, c) => (x.filter(_ > 0).reverse, c) }
        .filter(_._1.length == directions.length)
    } else {

      val newAcc = acc
        .flatMap { case (h :: t, c) =>
          (conditions(i) match {
            case Unknown if h == 0     => List(h :: t, h + 1 :: t)
            case Unknown               => List(0 :: h :: t, h + 1 :: t)
            case Operational if h == 0 => List(h :: t)
            case Operational           => List(0 :: h :: t)
            case Damaged               => List(h + 1 :: t)
          }).map(_ -> c)
        }
        .groupMapReduce(_._1)(_._2)(_ + _)

      val filtered = newAcc.filter { case (l, _) =>
        l.tail.reverseIterator.zip(directions).forall { case (c, d) =>
          c == d
        } &&
          (if (l.size > directions.size) l.head == 0 else l.head <= directions(l.size - 1))
      }

      generatePermutations(conditions, directions, filtered.toList, i + 1)
    }
  }

  def countPermutations(data: Seq[(Seq[Condition], Seq[Int])]): Long = {

    data.map { case (conditions, directions) =>
      val perms =
        generatePermutations(
          conditions.toIndexedSeq,
          directions.toIndexedSeq,
          List((0 :: Nil) -> 1),
          0
        )

      val counts = perms.filter { case (p, _) =>
        p.length == directions.length && p.zip(directions).forall { case (c, d) => c == d }
      }

      counts.map(_._2).sum
    }.sum
  }

  def partOne(input: Seq[String]): Long = countPermutations(parseInput(input))

  def partTwo(input: Seq[String]): Long = {
    val data = parseInput(input).map { case (conditions, directions) =>
      1.to(4).foldLeft((conditions, directions)) { case ((c, d), _) =>
        (conditions ++ Seq[Condition](Unknown) ++ c, directions ++ d)
      }
    }

    countPermutations(data)
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
