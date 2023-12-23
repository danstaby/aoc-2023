package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Test extends AnyFlatSpec with Matchers {

  import Day23._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 2206
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 6490
  }
}
