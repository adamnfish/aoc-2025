package com.adamnfish.days

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day07Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "part1" - {
    "works on the example input" in {
      Day07
        .part1("example")
        .asserting(_ shouldBe "21")
    }
  }

  "part2" - {
    "works on the example input" in {
      Day07
        .part2("example")
        .asserting(_ shouldBe "40")
    }
  }
}
