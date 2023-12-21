package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Test extends AnyFlatSpec with Matchers {

  import Day21._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 3762
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 621944727930768L
  }
}
