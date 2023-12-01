package aoc

import scala.io.Source

trait Common {

  lazy val day: String = this.getClass.getName.toLowerCase match {
    case s"${_}.$x$$" => x
  }

  def readLines(name: String): Seq[String] = {
    val source = Source.fromFile(s"data/$day/$name")
    source.getLines().toSeq
  }

  lazy val problemInput: Seq[String] = readLines("input.txt")
  lazy val testInput: Seq[String] = readLines("test.txt")
}
