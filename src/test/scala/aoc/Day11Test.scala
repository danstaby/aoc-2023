package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {

  import Day11._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 9723824
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 731244261352L
  }
}
