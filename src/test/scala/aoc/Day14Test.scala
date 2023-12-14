package aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {

  import Day14._

  "partOne" should "be correct" in {
    partOne(problemInput) shouldBe 109466
  }

  "rotate" should "be commutative" in {
    val testData = Set(Point(2, 4), Point(8, 9))
    val (maxX, maxY) = (10, 15)

    def testFun(dir: Point): Set[Point] =
      inverseRotate(rotate(testData, dir, maxX, maxY), dir, maxX, maxY)

    testFun(Point.Up) shouldBe testData
    testFun(Point.Down) shouldBe testData
    testFun(Point.Left) shouldBe testData
    testFun(Point.Right) shouldBe testData
  }

  "partTwo" should "be correct for test input" in {
    partTwo(testInput) shouldBe 64
  }

  "partTwo" should "be correct" in {
    partTwo(problemInput) shouldBe 94585
  }
}
