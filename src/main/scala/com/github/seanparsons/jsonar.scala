package com.github.seanparsons

import scalaz._
import Scalaz._

package object jsonar {
  implicit val jsonValueEqual: Equal[JSONValue] = Equal.equalA[JSONValue]
  implicit val jsonNullEqual: Equal[JSONNull] = Equal.equalA[JSONNull]
  implicit val jsonIntEqual: Equal[JSONInt] = Equal.equalA[JSONInt]
  implicit val jsonDecimalEqual: Equal[JSONDecimal] = Equal.equalA[JSONDecimal]
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
  implicit val jsonIntMonoid: Monoid[JSONInt] = new Monoid[JSONInt] {
    def append(first: JSONInt, second: => JSONInt) = JSONInt(first.value |+| second.value)
    val zero = new JSONInt(0)
  }
  implicit val jsonDecimalMonoid: Monoid[JSONDecimal] = new Monoid[JSONDecimal] {
    def append(first: JSONDecimal, second: => JSONDecimal) = JSONDecimal(first.value |+| second.value)
    val zero = new JSONDecimal(0)
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

  def invalidConversionError[T](jsonValue: JSONValue)(implicit targetManifest: Manifest[T]): JSONError = InvalidConversionJSONError(jsonValue, targetManifest)
  def parseError(message: String): JSONError = JSONParseError(message)
  def subElementNotFoundError(jsonValue: JSONValue, elementName: String): JSONError = SubElementNotFoundJSONError(jsonValue, elementName)
}