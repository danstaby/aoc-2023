package aoc

object Day5 extends App with Common {

  case class MappingRange(destination: Range, source: Range)

  def parseInput(input: Seq[String]): Seq[Seq[String]] =
    input.mkString("\n").split("\n\n").map(_.split("\n").toSeq)

  def getSeeds(data: Seq[Seq[String]]): Seq[Long] = data.head.map { case s"seeds: $seeds" =>
    seeds.split(" ").map(_.toLong).toSeq
  }.head

  def getMaps(data: Seq[Seq[String]]): Seq[Seq[MappingRange]] =
    data.tail
      .map(_.tail.map { case s"$destination $source $length" =>
        MappingRange(
          Range(destination.toLong, destination.toLong + length.toLong),
          Range(source.toLong, source.toLong + length.toLong)
        )
      })

  def nextLayer(current: Seq[Long], ranges: Seq[MappingRange]): Seq[Long] = current
    .map { c =>
      ranges.find(_.source.contains(c)) match {
        case Some(r) => c - r.source.start + r.destination.start
        case None    => c
      }
    }

  def nextLayer(current: List[Range], ranges: Seq[MappingRange]): List[Range] = {
    val intersections =
      for { c <- current; r <- ranges; i <- r.source.intersection(c) } yield Range(
        i.start - r.source.start + r.destination.start,
        i.end - r.source.start + r.destination.start
      )

    val complements = ranges.foldLeft(current) { case (acc, r) =>
      acc.flatMap(_.relativeComplement(r.source)).filter(_.nonEmpty)
    }

    (intersections ++ complements)
      .sortBy(_.start)
      .foldLeft(List.empty[Range]) { case (acc, r) =>
        acc match {
          case h :: t if h.end >= r.start => Range(h.start, r.end) :: t
          case _                          => r :: acc
        }
      }
  }

  def partOne(input: Seq[String]): Long = {
    val data = parseInput(input)
    val seeds = getSeeds(data)

    getMaps(data).foldLeft(seeds) { case (current, ranges) => nextLayer(current, ranges) }.min
  }

  def partTwo(input: Seq[String]): Long = {
    val data = parseInput(input)
    val seeds = getSeeds(data).grouped(2).map { case Seq(a, b) => Range(a, a + b) }.toList

    getMaps(data)
      .foldLeft(seeds) { case (current, ranges) => nextLayer(current, ranges) }
      .map(_.start)
      .min
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
