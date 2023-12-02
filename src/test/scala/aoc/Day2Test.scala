package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFlatSpec with Matchers {
  import Day2._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 2512
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 67335
  }
}
