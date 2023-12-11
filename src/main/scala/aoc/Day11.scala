package aoc

object Day11 extends App with Common {

  def parseInput(input: Seq[String]): Set[Point] =
    input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (c, x) =>
        if (c == '#') Some(Point(x, y)) else None
      }
    }.toSet

  def partOne(input: Seq[String]): Int = {
    val data = parseInput(input)

    val emptyCols = 0.to(data.map(_.x).max).filterNot(data.map(_.x).contains)
    val emptyRows = 0.to(data.map(_.y).max).filterNot(data.map(_.y).contains)

    val stars = data.map { p =>
      Point(p.x + emptyCols.count(_ < p.x), p.y + emptyRows.count(_ < p.y))
    }

    val distances = for {
      star1 <- stars.toSeq
      star2 <- stars.toSeq
      if star1 != star2
    } yield star1.l1Norm(star2)

    distances.sum / 2
  }
  def partTwo(input: Seq[String]): Long = {

    val factor = 1000000 - 1

    val data = parseInput(input)

    val emptyCols = 0.to(data.map(_.x).max).filterNot(data.map(_.x).contains)
    val emptyRows = 0.to(data.map(_.y).max).filterNot(data.map(_.y).contains)

    val stars = data.map { p =>
      Point(p.x + factor * emptyCols.count(_ < p.x), p.y + factor * emptyRows.count(_ < p.y))
    }

    val distances =
      stars.toSeq.flatMap { s1 =>
        stars.toSeq.map(s2 =>
          math.abs(s1.x.toLong - s2.x.toLong) + math.abs(s1.y.toLong - s2.y.toLong)
        )
      }

    distances.sum / 2
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
