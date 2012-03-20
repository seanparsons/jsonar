package com.github.seanparsons

import jsonar.{JSONObject, JSONBool}
import scalaz._
import Scalaz._

package object jsonar {
  implicit val jsonValueEqual: Equal[JSONValue] = equalA
  implicit val jsonErrorEqual: Equal[JSONError] = equalA
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
  private[this] def safeCast[T <: JSONValue](instance: JSONValue)(implicit targetManifest: Manifest[T]): ValidationNEL[JSONError, T] = {
    if (targetManifest.erasure.isInstance(instance)) {
      instance.asInstanceOf[T].successNel
    } else {
      NotInstanceJSONError(instance, targetManifest).failNel
    }
  }
  protected[this] def subElementSearch(jsonValue: JSONValue, elementName: String): ValidationNEL[JSONError, JSONValue] = {
    val searchResult = jsonValue match {
      case jsonObject: JSONObject => jsonObject.fields.get(JSONString(elementName))
      case _ => none[JSONValue]
    }
    searchResult.map(_.successNel).getOrElse(SubElementNotFoundJSONError(jsonValue, elementName).failNel)
  }
  implicit def jsonValueToRichJSONValue(jsonValue: JSONValue): RichJSONValue = new RichJSONValue {
    def search(elementName: String) = subElementSearch(jsonValue, elementName)
    def \(elementName: String) = subElementSearch(jsonValue, elementName)
    def asJSONString = safeCast[JSONString](jsonValue)
    def asJSONInt = safeCast[JSONInt](jsonValue)
    def asJSONDecimal = safeCast[JSONDecimal](jsonValue)
    def asJSONBool = safeCast[JSONBool](jsonValue)
    def asJSONNull = safeCast[JSONNull](jsonValue)
    def asJSONArray = safeCast[JSONArray](jsonValue)
    def asJSONObject = safeCast[JSONObject](jsonValue)
  }
  implicit def validationJSONValueToRichJSONValue(validationJSONValue: ValidationNEL[JSONError, JSONValue]): RichJSONValue = new RichJSONValue {
    private[this] def foldJSONValidation[U <: JSONValue](successFold: (JSONValue) => ValidationNEL[JSONError, U])(implicit valueManifest: Manifest[U]): ValidationNEL[JSONError, U] = {
      validationJSONValue.fold(_.fail, successFold)
    }
    def search(elementName: String) = foldJSONValidation[JSONValue](subElementSearch(_, elementName))
    def \(elementName: String) = foldJSONValidation[JSONValue](subElementSearch(_, elementName))
    def asJSONString = foldJSONValidation[JSONString](safeCast)
    def asJSONInt = foldJSONValidation[JSONInt](safeCast)
    def asJSONDecimal = foldJSONValidation[JSONDecimal](safeCast)
    def asJSONBool = foldJSONValidation[JSONBool](safeCast)
    def asJSONNull = foldJSONValidation[JSONNull](safeCast)
    def asJSONArray = foldJSONValidation[JSONArray](safeCast)
    def asJSONObject = foldJSONValidation[JSONObject](safeCast)
  }
  private[this] def jsonIntToLong(jsonInt: JSONInt): ValidationNEL[JSONError, Long] = {
    if (jsonInt.value >= Long.MinValue && jsonInt.value <= Long.MaxValue) {
      jsonInt.value.longValue().successNel
    } else {
      InvalidConversionJSONError(jsonInt, implicitly[Manifest[Long]]).failNel
    }
  }
  private[this] def jsonIntToInt(jsonInt: JSONInt): ValidationNEL[JSONError, Int] = {
    if (jsonInt.value >= Int.MinValue && jsonInt.value <= Int.MaxValue) {
      jsonInt.value.intValue().successNel
    } else {
      InvalidConversionJSONError(jsonInt, implicitly[Manifest[Int]]).failNel
    }
  }
  private[this] def jsonIntToShort(jsonInt: JSONInt): ValidationNEL[JSONError, Short] = {
    if (jsonInt.value >= Short.MinValue && jsonInt.value <= Short.MaxValue) {
      jsonInt.value.shortValue().successNel
    } else {
      InvalidConversionJSONError(jsonInt, implicitly[Manifest[Short]]).failNel
    }
  }  
  implicit def jsonIntToRichJSONInt(jsonInt: JSONInt): RichJSONInt = new RichJSONInt {   
    def asLong() = jsonIntToLong(jsonInt)
    def asInt() = jsonIntToInt(jsonInt)
    def asShort() = jsonIntToShort(jsonInt)
    def asBigInt() = jsonInt.value.successNel
  }
  implicit def validationJSONIntToRichJSONInt(validation: ValidationNEL[JSONError, JSONInt]): RichJSONInt = new RichJSONInt {
    def asLong() = validation.flatMap(jsonIntToLong)
    def asInt() = validation.flatMap(jsonIntToInt)
    def asShort() = validation.flatMap(jsonIntToShort)
    def asBigInt() = validation.map(_.value)
  }
  implicit def jsonDecimalToRichJSONDecimal(jsonDecimal: JSONDecimal): RichJSONDecimal = new RichJSONDecimal {
    def asBigDecimal() = jsonDecimal.value.successNel
  }
  implicit def validationJSONDecimalToRichJSONDecimal(validation: ValidationNEL[JSONError, JSONDecimal]): RichJSONDecimal = new RichJSONDecimal {
    def asBigDecimal() = validation.map(_.value)
  }
  implicit def jsonStringToRichJSONString(jsonString: JSONString): RichJSONString = new RichJSONString {
    def asString() = jsonString.value.successNel
  }
  implicit def validationJSONStringToRichJSONString(validation: ValidationNEL[JSONError, JSONString]): RichJSONString = new RichJSONString {
    def asString() = validation.map(_.value)
  }
  implicit def jsonBoolToRichJSONBool(jsonBool: JSONBool): RichJSONBool = new RichJSONBool {
    def asBoolean() = jsonBool.value.successNel
  }
  implicit def validationJSONBoolToRichJSONBool(validation: ValidationNEL[JSONError, JSONBool]): RichJSONBool = new RichJSONBool {
    def asBoolean() = validation.map(_.value)
  }
  implicit def jsonArrayToRichJSONArray(jsonArray: JSONArray): RichJSONArray = new RichJSONArray {
    def asElements() = jsonArray.elements.successNel
    def collectElements[T](partialFunction: PartialFunction[JSONValue, T]) = jsonArray.elements.collect(partialFunction).successNel[JSONError]
  }
  implicit def validationJSONArrayToRichJSONArray(validation: ValidationNEL[JSONError, JSONArray]): RichJSONArray = new RichJSONArray {
    def asElements() = validation.map(_.elements)
    def collectElements[T](partialFunction: PartialFunction[JSONValue, T]) = validation.map(_.elements.collect(partialFunction))
  }
  implicit def jsonObjectToRichJSONObject(jsonObject: JSONObject): RichJSONObject = new RichJSONObject {
    def asFields() = jsonObject.fields.successNel
  }
  implicit def validationJSONObjectToRichJSONObject(validation: ValidationNEL[JSONError, JSONObject]): RichJSONObject = new RichJSONObject {
    def asFields() = validation.map(_.fields)
  }  
}