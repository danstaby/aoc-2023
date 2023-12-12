package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFlatSpec with Matchers {

  import Day12._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 6958
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 6555315065024L
  }
}
