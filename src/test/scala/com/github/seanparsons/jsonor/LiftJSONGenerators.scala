package com.github.seanparsons.jsonor

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import net.liftweb.json._
import org.scalacheck.Gen
import scalaz._
import Scalaz._

object LiftJSONGenerators {
  //def valueParser: Parser[JSONValue] = objectParser | arrayParser | stringParser | numberParser | nullParser | booleanParser
  val jintGenerator = arbitrary[Long].map(long => new JInt(long))
  val jdoubleGenerator = arbitrary[Double].suchThat(double => double > 1.0 || double < -1.0).map(double => new JDouble(double))
  val numberGenerator = jintGenerator //oneOf(jintGenerator, jdoubleGenerator)
  val stringGenerator = alphaStr.suchThat(_.length() > 0).map(string => new JString(string))
  val booleanGenerator = arbitrary[Boolean].map(boolean => new JBool(boolean))
  val nothingGenerator = value(JNothing)
  def simpleValueGenerator = oneOf(stringGenerator, numberGenerator, nothingGenerator, booleanGenerator)
  def complexValueGenerator(depth: Int): Gen[JValue] = oneOf(objectGenerator(depth - 1), arrayGenerator(depth - 1), stringGenerator, numberGenerator, nothingGenerator, booleanGenerator)
  def valueGenerator(depth: Int = 5): Gen[JValue] = if (depth <= 1) simpleValueGenerator else complexValueGenerator(depth - 1)
  def arrayGenerator(depth: Int): Gen[JArray] = listOf(valueGenerator(depth - 1)).map(entries => stabilise(new JArray(entries)))
  def objectGenerator(depth: Int): Gen[JObject] = arbImmutableMap(Arbitrary(alphaStr), Arbitrary(valueGenerator(depth - 1))).arbitrary
    .map(map => stabilise(new JObject(map.toList.map(item => new JField(item._1, item._2)))))
  val arrayOrObjectGenerator: Gen[JValue] = oneOf(arrayGenerator(5), objectGenerator(5))
    .map(jValue => jValue |> render |> compact |> parse) // Helpfully Lift-JSON trims out JNothing objects.

  def stabilise(jArray: JArray): JArray = jArray.copy(arr = jArray.arr.map(element => stabiliseJValue(element)))
  def stabilise(jObject: JObject): JObject = jObject
    .copy(obj = stabilise(jObject.obj).sortBy(_.name))
  def stabilise(jFields: List[JField]): List[JField] = {
    jFields.map(jField =>jField.copy(value = stabiliseJValue(jField.value))).sortBy(_.name)
  }

  def stabiliseJValue(jValue: JValue): JValue = {
    jValue match {
      case jArray: JArray => stabilise(jArray)
      case jObject: JObject => stabilise(jObject)
      case _ => jValue
    }
  }
}