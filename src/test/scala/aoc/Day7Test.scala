package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7Test extends AnyFlatSpec with Matchers {

  import Day7._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 249204891
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 249666369
  }
}
