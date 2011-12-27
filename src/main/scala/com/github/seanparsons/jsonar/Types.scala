package com.github.seanparsons.jsonar

sealed abstract class JSONValue
case object JSONNull extends JSONValue
case class JSONString(value: String) extends JSONValue
abstract class JSONNumber extends JSONValue
case class JSONDecimal(value: BigDecimal) extends JSONNumber {
  def this(value: Int) = this(BigDecimal(value))
  def this(value: Long) = this(BigDecimal(value))
  def this(value: Double) = this(BigDecimal(value))
  def this(value: Float) = this(BigDecimal(value))
}
case class JSONInt(value: BigInt) extends JSONNumber {
  def this(value: Int) = this(BigInt(value))
  def this(value: Long) = this(BigInt(value))
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
case class JSONObject(fields: Map[JSONString, JSONValue] = Map()) extends JSONValue {
  def this(fields: (JSONString, JSONValue)*) = this(fields.toMap)
}
object JSONObject {
  def apply(fields: (JSONString, JSONValue)*): JSONObject = new JSONObject(fields.toMap)
}
case class JSONArray(elements: Seq[JSONValue] = Seq()) extends JSONValue