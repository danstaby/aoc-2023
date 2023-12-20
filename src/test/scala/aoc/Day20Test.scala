package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Test extends AnyFlatSpec with Matchers {

  import Day20._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 788081152
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 224602011344203L
  }
}
