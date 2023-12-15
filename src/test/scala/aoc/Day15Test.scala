package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Test extends AnyFlatSpec with Matchers {

  import Day15._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 515495
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 229349
  }
}
