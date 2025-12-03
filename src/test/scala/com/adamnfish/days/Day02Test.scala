package com.adamnfish.days

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFreeSpec with Matchers {
  "validate" - {
    import Day02.*
    
    "part 1" - {
      val r = Day02.IsInvalidSillyPatterns

      "marks IDs that aren't a repeated sequence as Valid" in {
        validate(112345, r) shouldBe Id.Valid(112345)
        validate(123455, r) shouldBe Id.Valid(123455)
        validate(111122, r) shouldBe Id.Valid(111122)
        validate(122345, r) shouldBe Id.Valid(122345)
        validate(1, r) shouldBe Id.Valid(1)
      }

      "marks IDs that are entirely a repeated sequence as invalid" in {
        validate(123123, r) shouldBe Id.Invalid(123123)
        validate(11, r) shouldBe Id.Invalid(11)
        validate(4545, r) shouldBe Id.Invalid(4545)
        validate(1188511885, r) shouldBe Id.Invalid(1188511885)
      }
    }
  }
}
