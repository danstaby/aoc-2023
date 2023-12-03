package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Test extends AnyFlatSpec with Matchers {
  import Day3._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 546312
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 87449461
  }
}
