package com.adamnfish.days

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day08Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "part1" - {
    "works on the example input" in {
      Day08
        .part1("example")
        .asserting(_ shouldBe "40")
    }
  }

  "part2" - {
    "works on the example input" in {
      Day08
        .part2("example")
        .asserting(_ shouldBe "25272")
    }
  }
}
