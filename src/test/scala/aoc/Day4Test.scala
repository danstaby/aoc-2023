package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Test extends AnyFlatSpec with Matchers {

  import Day4._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 33950
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 14814534
  }
}
