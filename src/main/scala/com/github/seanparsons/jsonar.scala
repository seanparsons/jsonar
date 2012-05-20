package com.github.seanparsons

import scalaz._
import Scalaz._

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
    def append(first: JSONBool, second: => JSONBool) = if (first.value || second.value) JSONBoolTrue else JSONBoolFalse
    val zero = JSONBoolFalse
  }
  implicit val jsonObjectMonoid: Monoid[JSONObject] = new Monoid[JSONObject] {
    def append(first: JSONObject, second: => JSONObject) = JSONObject(first.fields ++ second.fields)
    val zero = JSONObject()
  }
  implicit val jsonArrayMonoid: Monoid[JSONArray] = new Monoid[JSONArray] {
    def append(first: JSONArray, second: => JSONArray) = JSONArray(first.elements ++ second.elements)
    val zero = JSONArray()
  }
  implicit def validationToJSONLike(validation: ValidationNEL[JSONError, JSONValue]): JSONLike = new JSONLike {
    def asJSONString: ValidationNEL[JSONError, JSONString] = validation.flatMap(_.asJSONString)
    def asJSONNumber: ValidationNEL[JSONError, JSONNumber] = validation.flatMap(_.asJSONNumber)
    def asJSONBool: ValidationNEL[JSONError, JSONBool] = validation.flatMap(_.asJSONBool)
    def asJSONNull: ValidationNEL[JSONError, JSONNull] = validation.flatMap(_.asJSONNull)
    def asJSONArray: ValidationNEL[JSONError, JSONArray] = validation.flatMap(_.asJSONArray)
    def asJSONObject: ValidationNEL[JSONError, JSONObject] = validation.flatMap(_.asJSONObject)
    def asOptionalJSONString: ValidationNEL[JSONError, Option[JSONString]] = validation.flatMap(_.asOptionalJSONString)
    def asOptionalJSONNumber: ValidationNEL[JSONError, Option[JSONNumber]] = validation.flatMap(_.asOptionalJSONNumber)
    def asOptionalJSONBool: ValidationNEL[JSONError, Option[JSONBool]] = validation.flatMap(_.asOptionalJSONBool)
    def asOptionalJSONArray: ValidationNEL[JSONError, Option[JSONArray]] = validation.flatMap(_.asOptionalJSONArray)
    def asOptionalJSONObject: ValidationNEL[JSONError, Option[JSONObject]] = validation.flatMap(_.asOptionalJSONObject)
    def asBigDecimal(): ValidationNEL[JSONError, BigDecimal] = validation.flatMap(_.asBigDecimal())
    def asBigInt(): ValidationNEL[JSONError, BigInt] = validation.flatMap(_.asBigInt())
    def asLong(): ValidationNEL[JSONError, Long] = validation.flatMap(_.asLong())
    def asInt(): ValidationNEL[JSONError, Int] = validation.flatMap(_.asInt())
    def asShort(): ValidationNEL[JSONError, Short] = validation.flatMap(_.asShort())
    def asByte(): ValidationNEL[JSONError, Byte] = validation.flatMap(_.asByte())
    def asString(): ValidationNEL[JSONError, String] = validation.flatMap(_.asString())
    def asBoolean(): ValidationNEL[JSONError, Boolean] = validation.flatMap(_.asBoolean())
    def /(elementName: String): ValidationNEL[JSONError, JSONValue] = validation.flatMap(_.asJSONString)
    def search(elementName: String): ValidationNEL[JSONError, JSONValue] = validation.flatMap(_.asJSONString)
    def asMap(): ValidationNEL[JSONError, Map[JSONString, JSONValue]] = validation.flatMap(_.asMap())
    def asSeq(): ValidationNEL[JSONError, Seq[JSONValue]] = validation.flatMap(_.asSeq())
  }

  def invalidConversionError[T](jsonValue: JSONValue)(implicit targetManifest: Manifest[T]): JSONError = InvalidConversionJSONError(jsonValue, targetManifest)
  def parseError(message: String): JSONError = JSONParseError(message)
  def subElementNotFoundError(jsonValue: JSONValue, elementName: String): JSONError = SubElementNotFoundJSONError(jsonValue, elementName)
}