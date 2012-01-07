package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

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
  def intValue: Option[Int] = if (value >= Int.MinValue && value <= Int.MaxValue) value.toInt.some else none
  def longValue: Option[Long] = if (value >= Long.MinValue && value <= Long.MaxValue) value.toLong.some else none
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
  def \(elementName: String): Validation[String, JSONValue]
  def findSubElement(elementName: String): Validation[String, JSONValue] = \(elementName)
  def asJSONNull: Validation[String, JSONNull]
  def asJSONString: Validation[String, JSONString]
  def asJSONInt: Validation[String, JSONInt]
  def asJSONDecimal: Validation[String, JSONDecimal]
  def asJSONBool: Validation[String, JSONBool]
  def asJSONObject: Validation[String, JSONObject]
  def asJSONArray: Validation[String, JSONArray]
}