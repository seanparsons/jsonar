package com.github.seanparsons.jsonar

import collection.TraversableLike
import collection.mutable.{MapBuilder, Builder}
import collection.immutable.VectorBuilder
import scalaz._
import Scalaz._
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

sealed abstract class JSONValue
case object JSONNull extends JSONValue
case class JSONString(value: String) extends JSONValue
abstract class JSONNumber extends JSONValue
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
abstract class JSONBool extends JSONValue {
  def value: Boolean
}
case object JSONBoolTrue extends JSONBool {
  val value = true
}
case object JSONBoolFalse extends JSONBool {
  val value = false
}
case class JSONObject(fields: Map[JSONString, JSONValue] = Map()) extends JSONValue with TraversableLike[(JSONString, JSONValue), JSONObject] {
  def this(fields: (JSONString, JSONValue)*) = this(fields.toMap)
  protected[jsonar] def newBuilder = new JSONObjectBuilder()
  def foreach[U](f: ((JSONString, JSONValue)) => U) = fields.foreach(f)
  def seq = fields
}
case class JSONObjectBuilder() extends Builder[(JSONString, JSONValue), JSONObject] {
  val mapBuilder = new MapBuilder[JSONString, JSONValue, Map[JSONString, JSONValue]](Map())
  def +=(elem: (JSONString, JSONValue)) = {
    mapBuilder += elem
    this
  }
  def clear() = mapBuilder.clear
  def result() = JSONObject(mapBuilder.result())
}
object JSONObject {
  def apply(fields: (JSONString, JSONValue)*): JSONObject = new JSONObject(fields.toMap)
}
case class JSONArray(elements: Seq[JSONValue] = Seq()) extends JSONValue with TraversableLike[JSONValue, JSONArray] {
  protected[jsonar] def newBuilder = new JSONArrayBuilder()
  def foreach[U](f: (JSONValue) => U) = elements.foreach(f)
  def seq = elements
}
case class JSONArrayBuilder() extends Builder[JSONValue, JSONArray] {
  val vectorBuilder = new VectorBuilder[JSONValue]()
  def +=(elem: JSONValue) = {
    vectorBuilder += elem
    this
  }
  def clear() = vectorBuilder.clear()
  def result() = JSONArray(vectorBuilder.result())
}