package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day22Test extends AnyFlatSpec with Matchers {

  import Day22._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 492
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 86556
  }
}
