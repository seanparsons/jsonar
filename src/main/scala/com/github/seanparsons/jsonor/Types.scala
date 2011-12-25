package com.github.seanparsons.jsonor

import scalaz._
import Scalaz._
import Stream._

sealed abstract class JSONValue {
  def textElements: Seq[String]
}
case object JSONNull extends JSONValue {
  val textElements = Seq("null")
}
case class JSONString(value: String) extends JSONValue {
  lazy val textElements = Seq("\"%s\"".format(value))
}
abstract class JSONNumber extends JSONValue
case class JSONDecimal(value: BigDecimal) extends JSONNumber{
  lazy val textElements = Seq(value.toString)
}
case class JSONInt(value: BigInt) extends JSONNumber{
  lazy val textElements = Seq(value.toString)
}
abstract class JSONBool extends JSONValue
case object JSONBoolTrue extends JSONBool {
  val value = true
  val textElements = Seq("true")
}
case object JSONBoolFalse extends JSONBool {
  val value = false
  val textElements = Seq("false")
}
case class JSONObject(fields: Map[JSONString, JSONValue]) extends JSONValue {
  //def textElements = fields.toSeq.flatMap(entry => entry._1.textElements :+ ":" ++ entry._2.textElements :+ ",").dropRight(1)
  def textElements = "{" +: fields.toSeq.flatMap(entry => (entry._1.textElements :+ ":") ++ entry._2.textElements :+ ",").dropRight(1) :+ "}"
}
case class JSONArray(elements: Seq[JSONValue]) extends JSONValue {
  def textElements = "[" +: elements.flatMap(element => element.textElements :+ ",").dropRight(1) :+ "]"
}