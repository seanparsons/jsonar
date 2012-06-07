package com.github.seanparsons

import scalaz._
import Scalaz._
import Lens._
import scalaz.std._
import Kleisli._
import Validation._

package object jsonar {
  implicit val jsonValueEqual: Equal[JSONValue] = Equal.equalA[JSONValue]
  implicit val jsonNullEqual: Equal[JSONNull] = Equal.equalA[JSONNull]
  implicit val jsonNumberEqual: Equal[JSONNumber] = Equal.equalA[JSONNumber]
  implicit val jsonStringEqual: Equal[JSONString] = Equal.equalA[JSONString]
  implicit val jsonBoolEqual: Equal[JSONBool] = Equal.equalA[JSONBool]
  implicit val jsonObjectEqual: Equal[JSONObject] = Equal.equalA[JSONObject]
  implicit val jsonArrayEqual: Equal[JSONArray] = Equal.equalA[JSONArray]
  implicit val jsonErrorEqual: Equal[JSONError] = Equal.equalA[JSONError]
  implicit val jsonStringOrder: Order[JSONString] = Order.orderBy((jsonString: JSONString) => jsonString.value)
  implicit val jsonNullMonoid: Monoid[JSONNull] = new Monoid[JSONNull] {
    def append(first: JSONNull, second: => JSONNull) = JSONNull
    val zero = JSONNull
  }
  implicit val jsonNumberMonoid: Monoid[JSONNumber] = new Monoid[JSONNumber] {
    def append(first: JSONNumber, second: => JSONNumber) = JSONNumber(first.value |+| second.value)
    val zero = new JSONNumber(0)
  }
  implicit val jsonStringMonoid: Monoid[JSONString] = new Monoid[JSONString] {
    def append(first: JSONString, second: => JSONString) = JSONString(first.value |+| second.value)
    val zero = new JSONString("")
  }
  implicit val jsonBoolMonoid: Monoid[JSONBool] = new Monoid[JSONBool] {
    def append(first: JSONBool, second: => JSONBool) = JSONBool(first.value || second.value)
    val zero = JSONBoolFalse
  }

  type PossibleJSONError[+T] = Validation[NonEmptyList[JSONError], T]
  type ValueConversion[From, To] = Kleisli[PossibleJSONError, From, To]

  implicit val bindPossibleJSONError: Bind[PossibleJSONError] = new Bind[PossibleJSONError]{
    def bind[A, B](fa: PossibleJSONError[A])(f: (A) => PossibleJSONError[B]) = fa.flatMap(f)
    def map[A, B](fa: PossibleJSONError[A])(f: (A) => B) = fa.map(f)
  }

  implicit def validationToJSONLike(validation: PossibleJSONError[JSONValue]): JSONLike = new JSONLike {
    def /(elementName: String): ValidationNEL[JSONError, JSONValue] = validation.flatMap(_ / elementName)
    def search(firstElementName: String, otherElementNames: String*): ValidationNEL[JSONError, JSONValue] = validation.flatMap(_.search(firstElementName, otherElementNames: _*))
    def as[T](implicit conversion: ValueConversion[JSONValue, T]): PossibleJSONError[T] = validation.flatMap(_.as[T])
    def asOptional[T](implicit conversion: ValueConversion[JSONValue, T], identifyNoneFrom: IdentifyNone[JSONValue]): PossibleJSONError[Option[T]] = validation.flatMap(_.asOptional[T])
  }

  implicit val jsonValueIdentifyNone: IdentifyNone[JSONValue] = new IdentifyNone[JSONValue] {
    def isNone(value: JSONValue) = value.foldNull(true, false)
  }

  implicit val jsonValueToJSONStringConversion: ValueConversion[JSONValue, JSONString] = kleisli[PossibleJSONError, JSONValue, JSONString]((from: JSONValue) => from.foldJSONString(_.successNel[JSONError], invalidConversionError[JSONString](from).failNel[JSONString]))
  implicit val jsonValueToJSONNullConversion: ValueConversion[JSONValue, JSONNull] = kleisli[PossibleJSONError, JSONValue, JSONNull]((from: JSONValue) => from.foldJSONNull(_.successNel[JSONError], invalidConversionError[JSONNull](from).failNel[JSONNull]))
  implicit val jsonValueToJSONBoolConversion: ValueConversion[JSONValue, JSONBool] = kleisli[PossibleJSONError, JSONValue, JSONBool]((from: JSONValue) => from.foldJSONBool(_.successNel[JSONError], invalidConversionError[JSONBool](from).failNel[JSONBool]))
  implicit val jsonValueToJSONNumberConversion: ValueConversion[JSONValue, JSONNumber] = kleisli[PossibleJSONError, JSONValue, JSONNumber]((from: JSONValue) => from.foldJSONNumber(_.successNel[JSONError], invalidConversionError[JSONNumber](from).failNel[JSONNumber]))
  implicit val jsonValueToJSONObjectConversion: ValueConversion[JSONValue, JSONObject] = kleisli[PossibleJSONError, JSONValue, JSONObject]((from: JSONValue) => from.foldJSONObject(_.successNel[JSONError], invalidConversionError[JSONObject](from).failNel[JSONObject]))
  implicit val jsonValueToJSONArrayConversion: ValueConversion[JSONValue, JSONArray] = kleisli[PossibleJSONError, JSONValue, JSONArray]((from: JSONValue) => from.foldJSONArray(_.successNel[JSONError], invalidConversionError[JSONArray](from).failNel[JSONArray]))

  @inline
  private[this] def convertTo[From, To](conversion: From => To)(implicit manifest: Manifest[To]): ValueConversion[From, To] = kleisli[PossibleJSONError, From, To]{(from: From) =>
    try {
      conversion(from).successNel
    } catch {
      case throwable => invalidConversionError[To](from).failNel
    }
  }

  implicit val jsonNumberToBigDecimalConversion: ValueConversion[JSONNumber, BigDecimal] = kleisli[PossibleJSONError, JSONNumber, BigDecimal]((from: JSONNumber) => from.value.successNel[JSONError])
  implicit val jsonNumberToBigIntConversion: ValueConversion[JSONNumber, BigInt] = convertTo[JSONNumber, BigInt](_.value.toBigInt)
  implicit val jsonNumberToLongConversion: ValueConversion[JSONNumber, Long] = convertTo[JSONNumber, Long](_.value.toLongExact)
  implicit val jsonNumberToIntConversion: ValueConversion[JSONNumber, Int] = convertTo[JSONNumber, Int](_.value.toIntExact)
  implicit val jsonNumberToShortConversion: ValueConversion[JSONNumber, Short] = convertTo[JSONNumber, Short](_.value.toShortExact)
  implicit val jsonNumberToByteConversion: ValueConversion[JSONNumber, Byte] = convertTo[JSONNumber, Byte](_.value.toByteExact)
  implicit val jsonValueToBigDecimalConversion: ValueConversion[JSONValue, BigDecimal] = jsonValueToJSONNumberConversion >=> jsonNumberToBigDecimalConversion
  implicit val jsonValueToBigIntConversion: ValueConversion[JSONValue, BigInt] = jsonValueToJSONNumberConversion >=> jsonNumberToBigIntConversion
  implicit val jsonValueToLongConversion: ValueConversion[JSONValue, Long] = jsonValueToJSONNumberConversion >=> jsonNumberToLongConversion
  implicit val jsonValueToIntConversion: ValueConversion[JSONValue, Int] = jsonValueToJSONNumberConversion >=> jsonNumberToIntConversion
  implicit val jsonValueToShortConversion: ValueConversion[JSONValue, Short] = jsonValueToJSONNumberConversion >=> jsonNumberToShortConversion
  implicit val jsonValueToByteConversion: ValueConversion[JSONValue, Byte] = jsonValueToJSONNumberConversion >=> jsonNumberToByteConversion

  implicit val jsonStringToStringConversion: ValueConversion[JSONString, String] = kleisli[PossibleJSONError, JSONString, String]((from: JSONString) => from.value.successNel[JSONError])
  implicit val jsonValueToStringConversion: ValueConversion[JSONValue, String] = jsonValueToJSONStringConversion >=> jsonStringToStringConversion

  def invalidConversionError[T](value: Any)(implicit targetManifest: Manifest[T]): JSONError = InvalidConversionJSONError(value, targetManifest)
  def parseError(message: String): JSONError = JSONParseError(message)
  def subElementNotFoundError(jsonValue: JSONValue, elementName: String): JSONError = SubElementNotFoundJSONError(jsonValue, elementName)
}