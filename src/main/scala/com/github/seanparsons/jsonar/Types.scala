package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import scalaz.std.stream
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

trait JSONLike {
  def asJSONString: ValidationNEL[JSONError, JSONString]
  def asJSONNumber: ValidationNEL[JSONError, JSONNumber]
  def asJSONBool: ValidationNEL[JSONError, JSONBool]
  def asJSONNull: ValidationNEL[JSONError, JSONNull]
  def asJSONArray: ValidationNEL[JSONError, JSONArray]
  def asJSONObject: ValidationNEL[JSONError, JSONObject]
  def asOptionalJSONString: ValidationNEL[JSONError, Option[JSONString]]
  def asOptionalJSONNumber: ValidationNEL[JSONError, Option[JSONNumber]]
  def asOptionalJSONBool: ValidationNEL[JSONError, Option[JSONBool]]
  def asOptionalJSONArray: ValidationNEL[JSONError, Option[JSONArray]]
  def asOptionalJSONObject: ValidationNEL[JSONError, Option[JSONObject]]
  def asBigDecimal(): ValidationNEL[JSONError, BigDecimal]
  def asBigInt(): ValidationNEL[JSONError, BigInt]
  def asLong(): ValidationNEL[JSONError, Long]
  def asInt(): ValidationNEL[JSONError, Int]
  def asShort(): ValidationNEL[JSONError, Short]
  def asByte(): ValidationNEL[JSONError, Byte]
  def asString(): ValidationNEL[JSONError, String]
  def asBoolean(): ValidationNEL[JSONError, Boolean]
  def asSeq(): ValidationNEL[JSONError, Seq[JSONValue]]
  def asMap(): ValidationNEL[JSONError, Map[JSONString, JSONValue]]
  def /(elementName: String): ValidationNEL[JSONError, JSONValue]
  def search(elementName: String): ValidationNEL[JSONError, JSONValue]
}

sealed abstract class JSONError {
  def message: String
}
case class JSONParseError(message: String) extends JSONError
case class NotInstanceJSONError[T <: JSONValue](value: JSONValue, target: Manifest[T]) extends JSONError {
  val message = "Cannot convert %s to %s.".format(value, target.erasure.getSimpleName)
}
case class SubElementNotFoundJSONError(value: JSONValue, elementName: String) extends JSONError {
  val message = "Could not find subelement \"%s\" in %s.".format(elementName, value)
}
case class InvalidConversionJSONError[T](value: JSONValue, target: Manifest[T]) extends JSONError {
  val message = "%s cannot be represented by %s.".format(value, target.erasure.getSimpleName)
}

sealed abstract class JSONValue extends JSONLike {
  def asJSONString(): ValidationNEL[JSONError, JSONString] = invalidConversionError(this).failNel
  def asJSONNumber(): ValidationNEL[JSONError, JSONNumber] = invalidConversionError(this).failNel
  def asJSONBool(): ValidationNEL[JSONError, JSONBool] = invalidConversionError(this).failNel
  def asJSONNull(): ValidationNEL[JSONError, JSONNull] = invalidConversionError(this).failNel
  def asJSONArray(): ValidationNEL[JSONError, JSONArray] = invalidConversionError(this).failNel
  def asJSONObject(): ValidationNEL[JSONError, JSONObject] = invalidConversionError(this).failNel
  def asOptionalJSONString(): ValidationNEL[JSONError, Option[JSONString]] = asJSONNull.map(_ => none).orElse(asJSONString.map(_.some))
  def asOptionalJSONNumber(): ValidationNEL[JSONError, Option[JSONNumber]] = asJSONNull.map(_ => none).orElse(asJSONNumber.map(_.some))
  def asOptionalJSONBool(): ValidationNEL[JSONError, Option[JSONBool]] = asJSONNull.map(_ => none).orElse(asJSONBool.map(_.some))
  def asOptionalJSONArray(): ValidationNEL[JSONError, Option[JSONArray]] = asJSONNull.map(_ => none).orElse(asJSONArray.map(_.some))
  def asOptionalJSONObject(): ValidationNEL[JSONError, Option[JSONObject]] = asJSONNull.map(_ => none).orElse(asJSONObject.map(_.some))

  // JSONNumber
  def asBigDecimal(): ValidationNEL[JSONError, BigDecimal] = invalidConversionError(this).failNel
  def asBigInt(): ValidationNEL[JSONError, BigInt] = invalidConversionError(this).failNel
  def asLong(): ValidationNEL[JSONError, Long] = invalidConversionError(this).failNel
  def asInt(): ValidationNEL[JSONError, Int] = invalidConversionError(this).failNel
  def asShort(): ValidationNEL[JSONError, Short] = invalidConversionError(this).failNel
  def asByte(): ValidationNEL[JSONError, Byte] = invalidConversionError(this).failNel

  // JSONString
  def asString(): ValidationNEL[JSONError, String] = invalidConversionError(this).failNel

  // JSONBool
  def asBoolean(): ValidationNEL[JSONError, Boolean] = invalidConversionError(this).failNel

  // JSONObject
  def asMap(): ValidationNEL[JSONError, Map[JSONString, JSONValue]] = invalidConversionError(this).failNel
  def /(elementName: String): ValidationNEL[JSONError, JSONValue] = subElementNotFoundError(this, elementName).failNel
  def search(elementName: String): ValidationNEL[JSONError, JSONValue] = subElementNotFoundError(this, elementName).failNel

  // JSONArray
  def asSeq(): ValidationNEL[JSONError, Seq[JSONValue]] = invalidConversionError(this).failNel
}
sealed abstract class JSONNull extends JSONValue
case object JSONNull extends JSONNull {
  override def asJSONNull(): ValidationNEL[JSONError, JSONNull] = this.successNel
}
case class JSONString(value: String) extends JSONValue {
  override def asJSONString(): ValidationNEL[JSONError, JSONString] = this.successNel
  override def asString: ValidationNEL[JSONError, String] = value.successNel
}
case class JSONNumber(value: BigDecimal) extends JSONValue {
  def this(value: JavaBigDecimal) = this(new BigDecimal(value))
  def this(value: BigInt) = this(new JavaBigDecimal(value.bigInteger))
  def this(value: Int) = this(BigDecimal(value))
  def this(value: Long) = this(BigDecimal(value))
  def this(value: Double) = this(BigDecimal(value))
  def this(value: Float) = this(BigDecimal(value))
  def this(value: Short) = this(BigDecimal(value))
  def this(value: Byte) = this(BigDecimal(value))
  @inline
  private[this] def convertTo[T](conversion: => T)(implicit manifest: Manifest[T]): ValidationNEL[JSONError, T] = {
    try {
      conversion.successNel
    } catch {
      case throwable => invalidConversionError[T](this).failNel
    }
  }
  override def asJSONNumber(): ValidationNEL[JSONError, JSONNumber] = this.successNel
  override def asBigDecimal(): ValidationNEL[JSONError, BigDecimal] = value.successNel
  override def asBigInt(): ValidationNEL[JSONError, BigInt] = convertTo(value.toBigInt)
  override def asLong(): ValidationNEL[JSONError, Long] = convertTo(value.toLongExact)
  override def asInt(): ValidationNEL[JSONError, Int] = convertTo(value.toIntExact)
  override def asShort(): ValidationNEL[JSONError, Short] = convertTo(value.toShortExact)
  override def asByte(): ValidationNEL[JSONError, Byte] = convertTo(value.toByteExact)
}
sealed abstract class JSONBool extends JSONValue {
  def value: Boolean
  override def asJSONBool: ValidationNEL[JSONError, JSONBool] = this.successNel
  override def asBoolean(): ValidationNEL[JSONError, Boolean] = value.successNel
}
case object JSONBoolTrue extends JSONBool {
  val value = true
}
case object JSONBoolFalse extends JSONBool {
  val value = false
}
case class JSONObject(fields: Map[JSONString, JSONValue] = Map()) extends JSONValue with PartialFunction[JSONString, JSONValue] {
  def this(fields: (JSONString, JSONValue)*) = this(fields.toMap)

  override def /(elementName: String): ValidationNEL[JSONError, JSONValue] = search(elementName)
  override def search(elementName: String): ValidationNEL[JSONError, JSONValue] = {
    fields
      .get(JSONString(elementName))
      .map(jsonValue => jsonValue.successNel[JSONError])
      .getOrElse(subElementNotFoundError(this, elementName).failNel[JSONValue])
  }

  override def asJSONObject(): ValidationNEL[JSONError, JSONObject] = this.successNel
  override def asMap(): ValidationNEL[JSONError, Map[JSONString, JSONValue]] = fields.successNel
  override def apply(key: JSONString) = fields(key)
  def isDefinedAt(key: JSONString) = fields.isDefinedAt(key)
  override def toString() = "JSONObject(%s)".format(fields)
}
object JSONObject {
  def apply(fields: (JSONString, JSONValue)*): JSONObject = new JSONObject(fields.toMap)
}
case class JSONArray(elements: Seq[JSONValue] = Seq()) extends JSONValue with Traversable[JSONValue] with PartialFunction[Int, JSONValue] {
  def isDefinedAt(key: Int) = elements.isDefinedAt(key)
  def this(first: JSONValue, rest: JSONValue*) = this(first +: rest)

  override def asJSONArray(): ValidationNEL[JSONError, JSONArray] = this.successNel
  override def asSeq(): ValidationNEL[JSONError, Seq[JSONValue]] = elements.successNel
  override def apply(position: Int) = elements(position)
  def foreach[U](f: (JSONValue) => U) = elements.foreach(f)
  override def toString() = "JSONArray(%s)".format(elements)
}
object JSONArray {
  def apply(first: JSONValue, rest: JSONValue*): JSONArray = new JSONArray(first +: rest)
}