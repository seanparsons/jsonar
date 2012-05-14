package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import scalaz.std.stream
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

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

sealed abstract class JSONValue {
  private[this] def safeCast[T <: JSONValue](implicit targetManifest: Manifest[T]): ValidationNEL[JSONError, T] = {
    if (targetManifest.erasure.isInstance(this)) {
      this.asInstanceOf[T].successNel
    } else {
      val error: JSONError = NotInstanceJSONError(this, targetManifest);
      error.failNel
    }
  }
  def asJSONString = safeCast[JSONString]
  def asJSONInt = safeCast[JSONInt]
  def asJSONDecimal = safeCast[JSONDecimal]
  def asJSONBool = safeCast[JSONBool]
  def asJSONNull = safeCast[JSONNull]
  def asJSONArray = safeCast[JSONArray]
  def asJSONObject = safeCast[JSONObject]
}
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
  def asLong(): ValidationNEL[JSONError, Long] = {
    if (value >= Long.MinValue && value <= Long.MaxValue) {
      value.longValue().successNel
    } else {
      invalidConversionError[Long](this).failNel
    }
  }
  def asInt(): ValidationNEL[JSONError, Int] = {
    if (value >= Int.MinValue && value <= Int.MaxValue) {
      value.intValue().successNel
    } else {
      invalidConversionError[Int](this).failNel
    }
  }
  def asShort(): ValidationNEL[JSONError, Short] = {
    if (value >= Short.MinValue && value <= Short.MaxValue) {
      value.shortValue().successNel
    } else {
      invalidConversionError[Short](this).failNel
    }
  }

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

  def \(elementName: String): ValidationNEL[JSONError, JSONValue] = search(elementName)
  def search(elementName: String): ValidationNEL[JSONError, JSONValue] = {
    fields
      .get(JSONString(elementName))
      .map(jsonValue => jsonValue.successNel[JSONError])
      .getOrElse(subElementNotFoundError(this, elementName).failNel[JSONValue])
  }

  def toZipper(): Option[Zipper[(JSONString, JSONValue)]] = stream.toZipper(fields.toStream)
  def zipperEnd(): Option[Zipper[(JSONString, JSONValue)]] = stream.zipperEnd(fields.toStream)

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

  def toZipper(): Option[Zipper[JSONValue]] = stream.toZipper(elements.toStream)
  def zipperEnd(): Option[Zipper[JSONValue]] = stream.zipperEnd(elements.toStream)

  override def apply(key: Int) = elements(key)
  def foreach[U](f: (JSONValue) => U) = elements.foreach(f)
  override def toString() = "JSONArray(%s)".format(elements)
}
object JSONArray {
  def apply(first: JSONValue, rest: JSONValue*): JSONArray = new JSONArray(first +: rest)
}