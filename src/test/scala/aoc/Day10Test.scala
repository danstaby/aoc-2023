package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {

  import Day10._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 6815
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 269
  }
}
