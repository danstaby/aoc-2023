package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Test extends AnyFlatSpec with Matchers {
  import Day1.problemInput

  "Day1.partOne" should "be correct" in {
    Day1.partOne(problemInput) shouldBe 54561
  }

  "Day1.partTwo" should "be correct" in {
    Day1.partTwo(problemInput) shouldBe 54076
  }
}
