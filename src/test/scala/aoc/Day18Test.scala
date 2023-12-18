package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Test extends AnyFlatSpec with Matchers {

  import Day18._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 35991
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 54058824661845L
  }
}
