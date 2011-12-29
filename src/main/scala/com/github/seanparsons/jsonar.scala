package com.github.seanparsons

import jsonar.Parser.ParserError
import scalaz._
import Scalaz._
import collection.generic.CanBuildFrom

package object jsonar {
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
  implicit val jsonArrayCanBuildFrom = new CanBuildFrom[TraversableOnce[JSONValue], JSONValue, JSONArray] {
    def apply(from: TraversableOnce[JSONValue]) = apply()
    def apply() = new JSONArrayBuilder()
  }
  implicit val jsonObjectCanBuildFrom = new CanBuildFrom[TraversableOnce[(JSONString, JSONValue)], (JSONString, JSONValue), JSONObject] {
    def apply(from: TraversableOnce[(JSONString, JSONValue)]) = apply()
    def apply() = new JSONObjectBuilder()
  }
}