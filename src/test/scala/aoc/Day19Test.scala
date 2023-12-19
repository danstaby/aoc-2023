package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day19Test extends AnyFlatSpec with Matchers {

  import Day19._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 420739
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 130251901420382L
  }
}
