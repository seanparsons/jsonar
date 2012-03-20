package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

sealed abstract class JSONError {
  def message: String
}
case class JSONParseError(message: String) extends JSONError
case class NotInstanceJSONError[T <: JSONValue](value: JSONValue, target: Manifest[T]) extends JSONError {
  lazy val message = "Cannot convert %s to %s.".format(value, target.erasure.getSimpleName)
}
case class SubElementNotFoundJSONError(value: JSONValue, elementName: String) extends JSONError {
  lazy val message = "Could not find subelement \"%s\" in %s.".format(elementName, value)
}
case class InvalidConversionJSONError[T](value: JSONValue, target: Manifest[T]) extends JSONError {
  lazy val message = "%s cannot be represented by %s.".format(value, target.erasure.getSimpleName)
}


sealed abstract class JSONValue
sealed abstract class JSONNull extends JSONValue
case object JSONNull extends JSONNull
case class JSONString(value: String) extends JSONValue
sealed abstract class JSONNumber extends JSONValue
case class JSONDecimal(value: BigDecimal) extends JSONNumber {
  def this(value: JavaBigDecimal) = this(new BigDecimal(value))
  def this(value: Int) = this(BigDecimal(value))
  def this(value: Long) = this(BigDecimal(value))
  def this(value: Double) = this(BigDecimal(value))
  def this(value: Float) = this(BigDecimal(value))
}
case class JSONInt(value: BigInt) extends JSONNumber {
  def this(value: BigInteger) = this(new BigInt(value))
  def this(value: Int) = this(BigInt(value))
  def this(value: Long) = this(BigInt(value))
}
sealed abstract class JSONBool extends JSONValue {
  def value: Boolean
}
case object JSONBoolTrue extends JSONBool {
  val value = true
}
case object JSONBoolFalse extends JSONBool {
  val value = false
}
case class JSONObject(fields: Map[JSONString, JSONValue] = Map()) extends JSONValue with PartialFunction[JSONString, JSONValue] {
  def this(fields: (JSONString, JSONValue)*) = this(fields.toMap)
  def isDefinedAt(key: JSONString) = fields.isDefinedAt(key)
  override def apply(key: JSONString) = fields(key)
  override def toString() = "JSONObject(%s)".format(fields)
}
object JSONObject {
  def apply(fields: (JSONString, JSONValue)*): JSONObject = new JSONObject(fields.toMap)
}
case class JSONArray(elements: Seq[JSONValue] = Seq()) extends JSONValue with Traversable[JSONValue] with PartialFunction[Int, JSONValue] {
  def isDefinedAt(key: Int) = elements.isDefinedAt(key)
  def this(first: JSONValue, rest: JSONValue*) = this(first +: rest)
  override def apply(key: Int) = elements(key)
  def foreach[U](f: (JSONValue) => U) = elements.foreach(f)
  override def toString() = "JSONArray(%s)".format(elements)
}
object JSONArray {
  def apply(first: JSONValue, rest: JSONValue*): JSONArray = new JSONArray(first +: rest)
}
trait RichJSONValue {
  def search(elementName: String): ValidationNEL[JSONError, JSONValue]
  def \(elementName: String): ValidationNEL[JSONError, JSONValue]
  def findSubElement(elementName: String): ValidationNEL[JSONError, JSONValue] = \(elementName)
  def asJSONNull: ValidationNEL[JSONError, JSONNull]
  def asJSONString: ValidationNEL[JSONError, JSONString]
  def asJSONInt: ValidationNEL[JSONError, JSONInt]
  def asJSONDecimal: ValidationNEL[JSONError, JSONDecimal]
  def asJSONBool: ValidationNEL[JSONError, JSONBool]
  def asJSONObject: ValidationNEL[JSONError, JSONObject]
  def asJSONArray: ValidationNEL[JSONError, JSONArray]
}
trait RichJSONInt {
  def asLong(): ValidationNEL[JSONError, Long]
  def asInt(): ValidationNEL[JSONError, Int]
  def asShort(): ValidationNEL[JSONError, Short]
  def asBigInt(): ValidationNEL[JSONError, BigInt]
}
trait RichJSONDecimal {
  def asBigDecimal(): ValidationNEL[JSONError, BigDecimal]
}
trait RichJSONString {
  def asString(): ValidationNEL[JSONError, String]
}
trait RichJSONBool {
  def asBoolean(): ValidationNEL[JSONError, Boolean]
}
trait RichJSONArray {
  def asElements(): ValidationNEL[JSONError, Seq[JSONValue]]
  def collectElements[T](partialFunction: PartialFunction[JSONValue, T]): ValidationNEL[JSONError, Seq[T]]
}
trait RichJSONObject {
  def asFields(): ValidationNEL[JSONError, Map[JSONString, JSONValue]]
}