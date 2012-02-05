package com.github.seanparsons.jsonar

import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import org.scalacheck.Choose._
import org.scalacheck.Choose
import scalaz._
import Scalaz._
import scala.math.Numeric.Implicits
import org.specs2.{Specification, ScalaCheck}

case class JSONIntSpecification() extends Specification with ScalaCheck {
  def intValueSpec = 
    "intValue" ^
      "Any value between Int.MinValue and Int.MaxValue" ! 
      	forAllNoShrink(chooseNum(Int.MinValue, Int.MaxValue)){int =>
      	  val intValue = (new JSONInt(int)).intValue 
      	  ("intValue = " + intValue) |: intValue ≟ int.some
    	  }	^	
      "Positive value greater than Int.MaxValue" !
      	forAllNoShrink(chooseNum(Int.MaxValue.toLong + 1L, Long.MaxValue)){long =>
          val intValue = (new JSONInt(long)).intValue
          ("intValue = " + intValue) |: intValue ≟ None
        } ^
      "Negative value less than Int.MinValue" !
        forAllNoShrink(chooseNum(Long.MinValue, Int.MinValue.toLong - 1L)){long =>
          val intValue = (new JSONInt(long)).intValue
          ("intValue = " + intValue) |: intValue ≟ None
        } ^
      "Negative value less than Long.MinValue" ! {
        val intValue = (new JSONInt(TestValues.lowBigInt)).intValue
        ("intValue = " + intValue) |: intValue ≟ None
      } ^
      "Negative value greater than Long.MaxValue" ! {
        val intValue = (new JSONInt(TestValues.highBigInt)).intValue
        ("intValue = " + intValue) |: intValue ≟ None
      } ^ end
  def longValueSpec = 
    "longValue" ^
      "Any value between Long.MinValue and Long.MaxValue" !
        forAllNoShrink(chooseNum(Long.MinValue, Long.MaxValue)){long =>
          val longValue = (new JSONInt(long)).longValue
          ("longValue = " + longValue) |: longValue ≟ long.some
        } ^
      "Negative value less than Long.MinValue" ! {
        val longValue = (new JSONInt(TestValues.lowBigInt)).longValue
        ("longValue = " + longValue) |: longValue ≟ None
      } ^
      "Negative value greater than Long.MaxValue" ! {
        val longValue = (new JSONInt(TestValues.highBigInt)).longValue
        ("longValue = " + longValue) |: longValue ≟ None
      } ^ end

  def is = intValueSpec ^ longValueSpec
}