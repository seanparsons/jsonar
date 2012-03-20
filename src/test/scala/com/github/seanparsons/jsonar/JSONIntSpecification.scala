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
      	  val intValue = (new JSONInt(int)).asInt() 
      	  ("intValue = " + intValue) |: intValue ≟ int.successNel[JSONError]
    	  }	^	
      "Positive value greater than Int.MaxValue" !
      	forAllNoShrink(chooseNum(Int.MaxValue.toLong + 1L, Long.MaxValue)){long =>
      	  val jsonValue = new JSONInt(long)
          val intValue = jsonValue.asInt()
          ("intValue = " + intValue) |: intValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Int]]).failNel[Int]
        } ^
      "Negative value less than Int.MinValue" !
        forAllNoShrink(chooseNum(Long.MinValue, Int.MinValue.toLong - 1L)){long =>
          val jsonValue = new JSONInt(long)
          val intValue = jsonValue.asInt()
          ("intValue = " + intValue) |: intValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Int]]).failNel[Int]
        } ^
      "Negative value less than Long.MinValue" ! {
        val jsonValue = new JSONInt(TestValues.lowBigInt)
        val intValue = jsonValue.asInt()
        ("intValue = " + intValue) |: intValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Int]]).failNel[Int]
      } ^
      "Negative value greater than Long.MaxValue" ! {
        val jsonValue = new JSONInt(TestValues.highBigInt)
        val intValue = jsonValue.asInt()
        ("intValue = " + intValue) |: intValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Int]]).failNel[Int]
      } ^ end
  def longValueSpec = 
    "longValue" ^
      "Any value between Long.MinValue and Long.MaxValue" !
        forAllNoShrink(chooseNum(Long.MinValue, Long.MaxValue)){long =>
          val jsonValue = new JSONInt(long)
          val longValue = jsonValue.asLong()
          ("longValue = " + longValue) |: longValue ≟ long.successNel[JSONError]
        } ^
      "Negative value less than Long.MinValue" ! {
        val jsonValue = new JSONInt(TestValues.lowBigInt)
        val longValue = jsonValue.asLong()
        ("longValue = " + longValue) |: longValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Long]]).failNel[Long]
      } ^
      "Negative value greater than Long.MaxValue" ! {
        val jsonValue = new JSONInt(TestValues.highBigInt)
        val longValue = jsonValue.asLong()
        ("longValue = " + longValue) |: longValue ≟ InvalidConversionJSONError(jsonValue, implicitly[Manifest[Long]]).failNel[Long]
      } ^ end

  def is = intValueSpec ^ longValueSpec
}