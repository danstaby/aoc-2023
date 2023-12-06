package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6Test extends AnyFlatSpec with Matchers {

  import Day6._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 625968
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 43663323L
  }
}
