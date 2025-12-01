package com.adamnfish.days

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFreeSpec with Matchers {
  "countZeroesPassed" - {
    import Day01.*

    "counts zeroes passed when moving right" in {
      countZeroesPassed(98, Rotation(Direction.Right, 5)) shouldBe 1
      countZeroesPassed(95, Rotation(Direction.Right, 4)) shouldBe 0
      countZeroesPassed(99, Rotation(Direction.Right, 1)) shouldBe 1
      countZeroesPassed(50, Rotation(Direction.Right, 49)) shouldBe 0
      countZeroesPassed(0, Rotation(Direction.Right, 100)) shouldBe 1
      countZeroesPassed(0, Rotation(Direction.Right, 200)) shouldBe 2
      countZeroesPassed(4, Rotation(Direction.Right, 200)) shouldBe 2
      countZeroesPassed(4, Rotation(Direction.Right, 300)) shouldBe 3
      countZeroesPassed(98, Rotation(Direction.Right, 305)) shouldBe 4
      countZeroesPassed(0, Rotation(Direction.Right, 5)) shouldBe 0
    }

    "counts zeroes passed when moving left" in {
      countZeroesPassed(2, Rotation(Direction.Left, 5)) shouldBe 1
      countZeroesPassed(5, Rotation(Direction.Left, 4)) shouldBe 0
      countZeroesPassed(0, Rotation(Direction.Left, 1)) shouldBe 0
      countZeroesPassed(50, Rotation(Direction.Left, 50)) shouldBe 1
      countZeroesPassed(0, Rotation(Direction.Left, 100)) shouldBe 1
      countZeroesPassed(0, Rotation(Direction.Left, 200)) shouldBe 2
      countZeroesPassed(4, Rotation(Direction.Left, 200)) shouldBe 2
      countZeroesPassed(4, Rotation(Direction.Left, 300)) shouldBe 3
      countZeroesPassed(4, Rotation(Direction.Left, 305)) shouldBe 4
      countZeroesPassed(0, Rotation(Direction.Left, 5)) shouldBe 0
    }
  }
}
