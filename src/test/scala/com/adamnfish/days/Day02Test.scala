package com.adamnfish.days

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFreeSpec with Matchers {
  "validate" - {
    import Day02.*

    "marks IDs that aren't a repeated sequence as Valid" in {
      validate(112345) shouldBe Id.Valid(112345)
      validate(123455) shouldBe Id.Valid(123455)
      validate(111122) shouldBe Id.Valid(111122)
      validate(122345) shouldBe Id.Valid(122345)
      validate(1) shouldBe Id.Valid(1)
    }

    "marks IDs that are entirely a repeated sequence as invalid" in {
      validate(123123) shouldBe Id.Invalid(123123)
      validate(11) shouldBe Id.Invalid(11)
      validate(4545) shouldBe Id.Invalid(4545)
      validate(1188511885) shouldBe Id.Invalid(1188511885)
    }
  }
}
