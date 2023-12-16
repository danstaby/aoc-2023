package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {

  import Day16._

  "partOne" should "be correct for test input" in {
    partOne(testInput) shouldBe 46
  }

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 7307
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 7635
  }
}
