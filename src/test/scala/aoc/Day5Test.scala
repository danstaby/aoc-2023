package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Test extends AnyFlatSpec with Matchers {

  import Day5._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 174137457
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 1493866
  }
}
