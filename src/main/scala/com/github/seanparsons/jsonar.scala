package com.github.seanparsons

import jsonar.{JSONObject, JSONBool}
import scalaz._
import Scalaz._

package object jsonar {
  implicit def intToJSONInt(int: Int): JSONInt = JSONInt(int)
  implicit def longToJSONInt(long: Long): JSONInt = JSONInt(long)
  implicit def bigIntToJSONInt(bigInt: BigInt): JSONInt = JSONInt(bigInt)
  implicit def doubleToJSONDecimal(double: Double): JSONDecimal = JSONDecimal(double)
  implicit def floatToJSONDecimal(float: Float): JSONDecimal = JSONDecimal(float)
  implicit def bigDecimalToJSONDecimal(bigDecimal: BigDecimal): JSONDecimal = JSONDecimal(bigDecimal)
  implicit def stringToJSONString(string: String): JSONString = JSONString(string)
  implicit def jsonStringToString(jsonString: JSONString): String = jsonString.value
  implicit def jsonMapToJSONObject(map: Map[JSONString, JSONValue]): JSONObject = JSONObject(map)
  implicit def mapToJSONObject[T <% JSONString, U <% JSONValue](map: Map[T, U]): JSONObject = JSONObject(map.map(pair => (pair._1: JSONString, pair._2: JSONValue)))
  implicit def seqToJSONArray[T <% JSONValue](seq: Seq[T]): JSONArray = JSONArray(seq.map(item => item: JSONValue))
  implicit def jsonSeqToJSONArray(seq: Seq[JSONValue]): JSONArray = JSONArray(seq)
  implicit def normalTupleToJSONField[T <% JSONString, U <% JSONValue](keyValuePair: (T, U)): (JSONString, JSONValue) = (keyValuePair._1: T, keyValuePair._2: U)
  implicit val jsonValueEqual: Equal[JSONValue] = equalA
  implicit val jsonNullZero: Zero[JSONNull] = zero(JSONNull)
  implicit val jsonIntZero: Zero[JSONInt] = zero(JSONInt(0))
  implicit val jsonDecimalZero: Zero[JSONDecimal] = zero(JSONDecimal(0))
  implicit val jsonStringZero: Zero[JSONString] = zero(JSONString(""))
  implicit val jsonBoolZero: Zero[JSONBool] = zero(JSONBoolFalse)
  implicit val jsonObjectZero: Zero[JSONObject] = zero(JSONObject())
  implicit val jsonArrayZero: Zero[JSONArray] = zero(JSONArray())
  implicit val jsonNullSemigroup: Semigroup[JSONNull] = semigroup((a, b) => JSONNull)
  implicit val jsonIntSemigroup: Semigroup[JSONInt] = semigroup((a, b) => JSONInt(a.value |+| b.value))
  implicit val jsonDecimalSemigroup: Semigroup[JSONDecimal] = semigroup((a, b) => JSONDecimal(a.value + b.value))
  implicit val jsonStringSemigroup: Semigroup[JSONString] = semigroup((a, b) => JSONString(a.value |+| b.value))
  implicit val jsonBoolSemigroup: Semigroup[JSONBool] = semigroup((a, b) => if (a.value |+| b.value) JSONBoolTrue else JSONBoolFalse)
  implicit val jsonArraySemigroup: Semigroup[JSONArray] = semigroup((a, b) => JSONArray(a.elements |+| b.elements))
  private[this] def safeCast[T](instance: Any)(implicit targetManifest: Manifest[T]): ValidationNEL[String, T] = {
    if (targetManifest.erasure.isInstance(instance)) {
      instance.asInstanceOf[T].successNel
    } else {
      "Cannot convert %s to %s.".format(instance, targetManifest.erasure.getSimpleName).failNel
    }
  }
  protected[this] def subElementSearch(jsonValue: JSONValue, elementName: String): ValidationNEL[String, JSONValue] = {
    val searchResult = jsonValue match {
      case jsonObject: JSONObject => jsonObject.fields.get(elementName)
      case _ => none[JSONValue]
    }
    searchResult.map(_.successNel).getOrElse("Could not find subelement \"%s\" in %s.".format(elementName, jsonValue).failNel)
  }
  implicit def jsonValueToRichJSONValue(jsonValue: JSONValue): RichJSONValue = new RichJSONValue {
    def \(elementName: String) = subElementSearch(jsonValue, elementName)
    def asJSONString = safeCast[JSONString](jsonValue)
    def asJSONInt = safeCast[JSONInt](jsonValue)
    def asJSONDecimal = safeCast[JSONDecimal](jsonValue)
    def asJSONBool = safeCast[JSONBool](jsonValue)
    def asJSONNull = safeCast[JSONNull](jsonValue)
    def asJSONArray = safeCast[JSONArray](jsonValue)
    def asJSONObject = safeCast[JSONObject](jsonValue)
  }
  implicit def validationJSONValueToRichJSONValue[T](validationJSONValue: ValidationNEL[T, JSONValue])(implicit show: Show[T]): RichJSONValue = new RichJSONValue {
    private[this] def foldJSONValidation[U <: JSONValue](successFold: (JSONValue) => ValidationNEL[String, U])(implicit valueManifest: Manifest[U]): ValidationNEL[String, U] = {
      validationJSONValue.fold(_.shows.failNel, successFold)
    }
    def \(elementName: String) = foldJSONValidation[JSONValue](subElementSearch(_, elementName))
    def asJSONString = foldJSONValidation[JSONString](safeCast)
    def asJSONInt = foldJSONValidation[JSONInt](safeCast)
    def asJSONDecimal = foldJSONValidation[JSONDecimal](safeCast)
    def asJSONBool = foldJSONValidation[JSONBool](safeCast)
    def asJSONNull = foldJSONValidation[JSONNull](safeCast)
    def asJSONArray = foldJSONValidation[JSONArray](safeCast)
    def asJSONObject = foldJSONValidation[JSONObject](safeCast)
  }
}