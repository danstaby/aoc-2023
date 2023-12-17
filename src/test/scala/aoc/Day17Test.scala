package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {

  import Day17._

  "partOne" should "work with test data" in {
    partOne(testInput) shouldBe 102
  }

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 1099
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 1266
  }
}
