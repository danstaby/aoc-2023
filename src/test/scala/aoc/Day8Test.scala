package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day8Test extends AnyFlatSpec with Matchers {

  import Day8._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 24253
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 12357789728873L
  }
}
