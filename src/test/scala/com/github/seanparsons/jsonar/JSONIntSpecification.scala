package com.github.seanparsons.jsonar

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._

case class JSONIntSpecification() extends FeatureSpec
                                  with MustMatchers
                                  with Checkers {
  feature("intValue") {
    scenario("Any value between Int.MinValue and Int.MaxValue") {
      check(forAllNoShrink(chooseNum(Int.MinValue, Int.MaxValue)){int =>
        val intValue = (new JSONInt(int)).intValue
        ("intValue = " + intValue) |: intValue ≟ int.some
      })
    }
    scenario("Positive value greater than Int.MaxValue") {
      check(forAllNoShrink(chooseNum(Int.MaxValue.toLong + 1L, Long.MaxValue)){long =>
        val intValue = (new JSONInt(long)).intValue
        ("intValue = " + intValue) |: intValue ≟ none
      })
    }
    scenario("Negative value less than Int.MinValue") {
      check(forAllNoShrink(chooseNum(Long.MinValue, Int.MinValue.toLong - 1L)){long =>
        val intValue = (new JSONInt(long)).intValue
        ("intValue = " + intValue) |: intValue ≟ none
      })
    }
    scenario("Negative value less than Long.MinValue") {
      val intValue = (new JSONInt(TestValues.lowBigInt)).intValue
      ("intValue = " + intValue) |: intValue ≟ none
    }
    scenario("Negative value greater than Long.MaxValue") {
      val intValue = (new JSONInt(TestValues.highBigInt)).intValue
      ("intValue = " + intValue) |: intValue ≟ none
    }
  }
  feature("longValue") {
    scenario("Any value between Long.MinValue and Long.MaxValue") {
      check(forAllNoShrink(chooseNum(Long.MinValue, Long.MaxValue)){long =>
        val longValue = (new JSONInt(long)).longValue
        ("longValue = " + longValue) |: longValue ≟ long.some
      })
    }
    scenario("Negative value less than Long.MinValue") {
      val longValue = (new JSONInt(TestValues.lowBigInt)).longValue
      ("longValue = " + longValue) |: longValue ≟ none
    }
    scenario("Negative value greater than Long.MaxValue") {
      val longValue = (new JSONInt(TestValues.highBigInt)).longValue
      ("longValue = " + longValue) |: longValue ≟ none
    }
  }
}