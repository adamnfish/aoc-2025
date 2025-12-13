package com.adamnfish.days

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day05Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "part1" - {
    "works on the example input" in {
      Day05
        .part1("example")
        .asserting(_ shouldBe "3")
    }
  }

  "part2" - {
    "works on the example input" in {
      Day05
        .part2("example")
        .asserting(_ shouldBe "14")
    }
  }
}
