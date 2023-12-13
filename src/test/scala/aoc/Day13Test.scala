package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Test extends AnyFlatSpec with Matchers {

  import Day13._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 37025
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 32854
  }
}
