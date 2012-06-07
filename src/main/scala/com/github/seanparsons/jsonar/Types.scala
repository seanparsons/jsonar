package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import scalaz.std.stream
import java.math.BigInteger
import java.math.{BigDecimal => JavaBigDecimal}

trait IdentifyNone[T] {
  def isNone(value: T): Boolean
}

trait JSONLike {
  // TODO: Improve the whole optional mechanism through conversion typeclasses.
  def as[T](implicit conversion: ValueConversion[JSONValue, T]): PossibleJSONError[T]
  def asOptional[T](implicit conversion: ValueConversion[JSONValue, T], identifyNoneFrom: IdentifyNone[JSONValue]): PossibleJSONError[Option[T]]

  def /(elementName: String): PossibleJSONError[JSONValue]
  def search(firstElementName: String, otherElementNames: String*): PossibleJSONError[JSONValue]
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
case class InvalidConversionJSONError[T](value: Any, target: Manifest[T]) extends JSONError {
  val message = "%s cannot be represented by %s.".format(value, target.erasure.getSimpleName)
}

sealed abstract class JSONValue extends JSONLike {
  def /(elementName: String): ValidationNEL[JSONError, JSONValue] = subElementNotFoundError(this, elementName).failNel
  def search(firstElementName: String, otherElementNames: String*): ValidationNEL[JSONError, JSONValue] = (firstElementName +: otherElementNames).foldLeft(this.successNel[JSONError])(_ / _)

  def fold[X](jsonNull: => X,
              jsonBool: Boolean => X,
              jsonNumber: BigDecimal => X,
              jsonString: String => X,
              jsonArray: Seq[JSONValue] => X,
              jsonObject: Map[JSONString, JSONValue] => X
             ): X
  def foldNull[X](then: => X, otherwise: => X): X = otherwise
  def foldBool[X](then: Boolean => X, otherwise: => X): X = otherwise
  def foldNumber[X](then: BigDecimal => X, otherwise: => X): X = otherwise
  def foldString[X](then: String => X, otherwise: => X): X = otherwise
  def foldArray[X](then: Seq[JSONValue] => X, otherwise: => X): X = otherwise
  def foldObject[X](then: Map[JSONString, JSONValue] => X, otherwise: => X): X = otherwise
  def foldJSONNull[X](then: JSONNull => X, otherwise: => X): X = otherwise
  def foldJSONBool[X](then: JSONBool => X, otherwise: => X): X = otherwise
  def foldJSONNumber[X](then: JSONNumber => X, otherwise: => X): X = otherwise
  def foldJSONString[X](then: JSONString => X, otherwise: => X): X = otherwise
  def foldJSONArray[X](then: JSONArray => X, otherwise: => X): X = otherwise
  def foldJSONObject[X](then: JSONObject => X, otherwise: => X): X = otherwise

  def as[T](implicit conversion: ValueConversion[JSONValue, T]): PossibleJSONError[T] = conversion(this)
  def asOptional[T](implicit conversion: ValueConversion[JSONValue, T], identifyNoneFrom: IdentifyNone[JSONValue]): PossibleJSONError[Option[T]] = identifyNoneFrom.isNone(this) ? none[T].successNel[JSONError] | conversion(this).map(_.some)
}
sealed abstract class JSONNull extends JSONValue {
  override def foldNull[X](then: => X, otherwise: => X) = then
  override def foldJSONNull[X](then: (JSONNull) => X, otherwise: => X) = then(this)
  override def fold[X](jsonNull: => X,
                       jsonBool: Boolean => X,
                       jsonNumber: BigDecimal => X,
                       jsonString: String => X,
                       jsonArray: Seq[JSONValue] => X,
                       jsonObject: Map[JSONString, JSONValue] => X
                      ): X = jsonNull
}
case object JSONNull extends JSONNull {
}
case class JSONString(value: String) extends JSONValue {
  def fold[X](jsonNull: => X,
              jsonBool: (Boolean) => X,
              jsonNumber: (BigDecimal) => X,
              jsonString: (String) => X,
              jsonArray: (Seq[JSONValue]) => X,
              jsonObject: (Map[JSONString, JSONValue]) => X
             ): X = jsonString(value)

  override def foldString[X](then: (String) => X, otherwise: => X) = then(value)
  override def foldJSONString[X](then: (JSONString) => X, otherwise: => X) = then(this)
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

  def fold[X](jsonNull: => X,
              jsonBool: (Boolean) => X,
              jsonNumber: (BigDecimal) => X,
              jsonString: (String) => X,
              jsonArray: (Seq[JSONValue]) => X,
              jsonObject: (Map[JSONString, JSONValue]) => X
             ): X = jsonNumber(value)
  override def foldNumber[X](then: (BigDecimal) => X, otherwise: => X) = then(value)
  override def foldJSONNumber[X](then: (JSONNumber) => X, otherwise: => X) = then(this)
}
object JSONBool {
  def apply(bool: Boolean): JSONBool = bool ? (JSONBoolTrue: JSONBool) | (JSONBoolFalse: JSONBool)
  def unapply(jsonValue: JSONValue): Option[Boolean] = jsonValue.foldBool(some[Boolean], none[Boolean])
}
sealed abstract class JSONBool extends JSONValue {
  def value: Boolean

  def fold[X](jsonNull: => X,
              jsonBool: (Boolean) => X,
              jsonNumber: (BigDecimal) => X,
              jsonString: (String) => X,
              jsonArray: (Seq[JSONValue]) => X,
              jsonObject: (Map[JSONString, JSONValue]) => X
             ): X = jsonBool(value)
  override def foldBool[X](then: (Boolean) => X, otherwise: => X) = then(value)
  override def foldJSONBool[X](then: (JSONBool) => X, otherwise: => X) = then(this)
}
case object JSONBoolTrue extends JSONBool {
  val value = true
}
case object JSONBoolFalse extends JSONBool {
  val value = false
}
case class JSONObject(fields: Map[JSONString, JSONValue] = Map()) extends JSONValue {
  def this(fields: (JSONString, JSONValue)*) = this(fields.toMap)
  def fold[X](jsonNull: => X,
              jsonBool: (Boolean) => X,
              jsonNumber: (BigDecimal) => X,
              jsonString: (String) => X,
              jsonArray: (Seq[JSONValue]) => X,
              jsonObject: (Map[JSONString, JSONValue]) => X
             ): X = jsonObject(fields)

  override def foldObject[X](then: (Map[JSONString, JSONValue]) => X, otherwise: => X) = then(fields)
  override def foldJSONObject[X](then: (JSONObject) => X, otherwise: => X) = then(this)
  override def /(elementName: String): ValidationNEL[JSONError, JSONValue] = fields.get(JSONString(elementName)).fold(_.successNel, subElementNotFoundError(this, elementName).failNel)
}
object JSONObject {
  def apply(fields: (JSONString, JSONValue)*): JSONObject = new JSONObject(fields.toMap)
}
case class JSONArray(elements: Seq[JSONValue] = Seq()) extends JSONValue {
  def this(first: JSONValue, rest: JSONValue*) = this(first +: rest)
  def fold[X](jsonNull: => X,
              jsonBool: (Boolean) => X,
              jsonNumber: (BigDecimal) => X,
              jsonString: (String) => X,
              jsonArray: (Seq[JSONValue]) => X,
              jsonObject: (Map[JSONString, JSONValue]) => X
             ): X = jsonArray(elements)

  override def foldArray[X](then: (Seq[JSONValue]) => X, otherwise: => X) = then(elements)
  override def foldJSONArray[X](then: (JSONArray) => X, otherwise: => X) = then(this)
}
object JSONArray {
  def apply(first: JSONValue, rest: JSONValue*): JSONArray = new JSONArray(first +: rest)
}