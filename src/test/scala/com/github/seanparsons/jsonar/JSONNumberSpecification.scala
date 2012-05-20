package com.github.seanparsons.jsonar

import org.scalacheck.Prop._
import org.scalacheck.Gen.{value => _, _}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.{arbBigDecimal => _, _}
import org.scalacheck.Choose._
import scalaz._
import Scalaz._
import org.specs2.{Specification, ScalaCheck}
import org.specs2.specification._
import JSONGenerators._

case class JSONNumberSpecification() extends Specification with ScalaCheck {
  def createNumberValueSpec[T](name: String,
                               retrieveValue: (JSONNumber) => ValidationNEL[JSONError, T],
                               validRange: Gen[T],
                               validBuilder: (T) => JSONNumber,
                               invalidUpperRange: Option[Gen[BigDecimal]],
                               invalidLowerRange: Option[Gen[BigDecimal]])
                              (implicit equalT: Equal[T], manifest: Manifest[T]): Fragments = {

    val parts: Seq[Option[Example]] =
      Seq(
        ("Any value in the valid range" !
          forAllNoShrink(validRange){originalValue =>
            val potentiallyOriginalValue = retrieveValue(validBuilder(originalValue))
            ("potentiallyOriginalValue = " + potentiallyOriginalValue) |: potentiallyOriginalValue === originalValue.successNel[JSONError]
          }).some,
        invalidUpperRange.map{upperRange =>
          "Value too high to represent" !
            forAllNoShrink(upperRange){bigDecimal =>
              val jsonValue = new JSONNumber(bigDecimal)
              val expectedConversionError: ValidationNEL[JSONError, T] = retrieveValue(jsonValue)
              ("expectedConversionError = " + expectedConversionError) |: expectedConversionError === invalidConversionError[T](jsonValue).failNel[T]
            }
        },
        invalidLowerRange.map{lowerRange =>
          "Value too low to represent" !
            forAllNoShrink(lowerRange){bigDecimal =>
              val jsonValue = new JSONNumber(bigDecimal)
              val expectedConversionError: ValidationNEL[JSONError, T] = retrieveValue(jsonValue)
              ("expectedConversionError = " + expectedConversionError) |: expectedConversionError === invalidConversionError[T](jsonValue).failNel[T]
          }
        }
      )
     parts.flatten.foldLeft(name: Fragments)(_ ^ _) ^ end
  }

  def byteValueSpec = createNumberValueSpec[Byte](
    "byteValue",
    (jsonNumber) => jsonNumber.asByte(),
    chooseNum(Byte.MinValue, Byte.MaxValue),
    (byte) => new JSONNumber(BigDecimal(byte)),
    (arbitrary[BigDecimal].filter(_ > BigDecimal(Byte.MaxValue))).some,
    (arbitrary[BigDecimal].filter(_ < BigDecimal(Byte.MinValue))).some)
  def shortValueSpec = createNumberValueSpec[Short](
    "shortValue",
    (jsonNumber) => jsonNumber.asShort(),
    chooseNum(Short.MinValue, Short.MaxValue),
    (short) => new JSONNumber(BigDecimal(short)),
    (arbitrary[BigDecimal].filter(_ > BigDecimal(Short.MaxValue))).some,
    (arbitrary[BigDecimal].filter(_ < BigDecimal(Short.MinValue))).some)
  def intValueSpec = createNumberValueSpec[Int](
    "intValue",
    (jsonNumber) => jsonNumber.asInt(),
    chooseNum(Int.MinValue, Int.MaxValue),
    (int) => new JSONNumber(BigDecimal(int)),
    (arbitrary[BigDecimal].filter(_ > BigDecimal(Int.MaxValue))).some,
    (arbitrary[BigDecimal].filter(_ < BigDecimal(Int.MinValue))).some)
  def longValueSpec = createNumberValueSpec[Long](
    "longValue",
    (jsonNumber) => jsonNumber.asLong(),
    chooseNum(Long.MinValue, Long.MaxValue),
    (long) => new JSONNumber(BigDecimal(long)),
    (posNum[Long].map(long => BigDecimal(long + 1) + BigDecimal(Long.MaxValue))).some,
    (negNum[Long].map(long => BigDecimal(long - 1) + BigDecimal(Long.MinValue))).some)
  def bigIntValueSpec = createNumberValueSpec[BigInt](
    "bigIntValue",
    (jsonNumber) => jsonNumber.asBigInt(),
    arbitrary[Long].map(long => BigInt(long)),  // Ensure this value can be represented by a BigDecimal.
    (bigInt) => new JSONNumber(BigDecimal(bigInt)),
    None,
    None)
  def bigDecimalValueSpec = createNumberValueSpec[BigDecimal](
    "bigDecimalValue",
    (jsonNumber) => jsonNumber.asBigDecimal(),
    arbitrary[BigDecimal],
    (bigDecimal) => new JSONNumber(bigDecimal),
    None,
    None)

  def is = args.report(failtrace = true) ^ byteValueSpec ^ shortValueSpec ^ intValueSpec ^ longValueSpec ^ bigIntValueSpec ^ bigDecimalValueSpec
}