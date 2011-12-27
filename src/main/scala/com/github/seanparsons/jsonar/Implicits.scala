package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import com.github.seanparsons.jsonar.Parser.ParserError

object Implicits {
  implicit def intToJSONInt(int: Int): JSONInt = JSONInt(int)
  implicit def longToJSONInt(long: Long): JSONInt = JSONInt(long)
  implicit def bigIntToJSONInt(bigInt: BigInt): JSONInt = JSONInt(bigInt)
  implicit def doubleToJSONDecimal(double: Double): JSONDecimal = JSONDecimal(double)
  implicit def floatToJSONDecimal(float: Float): JSONDecimal = JSONDecimal(float)
  implicit def bigDecimalToJSONDecimal(bigDecimal: BigDecimal): JSONDecimal = JSONDecimal(bigDecimal)
  implicit def stringToJSONString(string: String): JSONString = JSONString(string)
  implicit def mapToJSONObject(map: Map[JSONString, JSONValue]): JSONObject = JSONObject(map)
  implicit def seqToJSONArray(seq: Seq[JSONValue]): JSONArray = JSONArray(seq)
  implicit val jsonValueEqual: Equal[JSONValue] = equalA
  implicit val jsonValidationEqual: Equal[ValidationNEL[ParserError, JSONValue]] = equalA
}