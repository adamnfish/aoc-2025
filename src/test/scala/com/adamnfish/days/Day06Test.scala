package com.adamnfish.days

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day06Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "part1" - {
    "works on the example input" in {
      Day06
        .part1("example")
        .asserting(_ shouldBe "4277556")
    }
  }

  "part2" - {
    "works on the example input" in {
      Day06
        .part2("example")
        .asserting(_ shouldBe "1")
    }
  }
}
