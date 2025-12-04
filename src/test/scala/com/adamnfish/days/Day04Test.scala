package com.adamnfish.days

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AsyncFreeSpec

class Day04Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "part1" - {
    "works on the example input" in {
      Day04
        .part1("example")
        .asserting(_ shouldBe "13")
    }
  }
}
