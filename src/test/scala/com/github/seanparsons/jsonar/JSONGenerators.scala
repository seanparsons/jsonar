package com.github.seanparsons.jsonar

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object JSONGenerators {
  def codePointStream(string: String): Stream[Int] = {
    def codePointStream(offset: Int): Stream[Int] = {
      if (offset > string.length - 1) Stream.empty
      else {
        val codePoint = string.codePointAt(offset) 
        Stream.cons(codePoint, codePointStream(offset + Character.charCount(codePoint)))
      }
    }
    codePointStream(0)
  }
  def isValidUnicodeCodePoint(codePoint: Int): Boolean = {
    Character.isLetterOrDigit(codePoint) || Character.isWhitespace(codePoint) || Character.isISOControl(codePoint)
  }
  val intGenerator: Gen[StringBuilder] = arbitrary[Long].map(long => new StringBuilder().append(long))
  val jsonIntGenerator: Gen[JSONInt] = arbitrary[Long].map(long => JSONInt(long))
  val doubleGenerator: Gen[StringBuilder] = arbitrary[Double].map(double => new StringBuilder().append(double))
  val jsonDoubleGenerator: Gen[JSONDecimal] = arbitrary[Double].map(double => JSONDecimal(double))
  val numberGenerator: Gen[StringBuilder] = oneOf(intGenerator, doubleGenerator)
  val jsonNumberGenerator: Gen[JSONNumber] = oneOf(jsonIntGenerator, jsonDoubleGenerator)
  val stringGenerator: Gen[StringBuilder] = arbitrary[String].map{string =>
    val codePoints = codePointStream(string).filter(isValidUnicodeCodePoint)
    val builder = codePoints.foldLeft(new java.lang.StringBuilder().append('"')){(builder, codePoint) =>
      if (codePoint <= 0xffff) {
        builder.append(codePoint.toChar)
      } else {
        builder.appendCodePoint(codePoint)
      }
    }.append('"')
    new StringBuilder().append(builder)
  }
  val jsonStringGenerator: Gen[JSONString] = stringGenerator.map(stringBuilder => JSONString(stringBuilder.toString))
  val booleanGenerator: Gen[StringBuilder] = arbitrary[Boolean].map(boolean => new StringBuilder().append(if (boolean) "true" else "false"))
  val jsonBoolGenerator: Gen[JSONBool] = oneOf(JSONBoolTrue, JSONBoolFalse)
  val nothingGenerator: Gen[StringBuilder] = value(new StringBuilder().append("null"))
  val jsonNothingGenerator: Gen[JSONValue] = value(JSONNull)
  def arrayGenerator(depth: Int = 5): Gen[StringBuilder] = listOfN(5, valueGenerator(depth - 1)).map{values => 
    val builder = new StringBuilder()
    builder.append('[')
    values.headOption.foreach(builder.append)
    if (!values.isEmpty) {
      values.tail.foreach{tailElement =>
        builder.append(',')
        builder.append(tailElement)
      }
    }
    builder.append(']')
  }
  def jsonArrayItemsGenerator(depth: Int = 5): Gen[Seq[JSONValue]] = listOfN(5, jsonValueGenerator(depth - 1))
  def jsonArrayGenerator(depth: Int = 5): Gen[JSONArray] = jsonArrayItemsGenerator(depth).map{values => JSONArray(values)}
  def objectGenerator(depth: Int = 5): Gen[StringBuilder] = arbImmutableMap(Arbitrary(stringGenerator), Arbitrary(valueGenerator(depth - 1))).arbitrary.map{map =>
    val builder = new StringBuilder()
    def addPair(builder: StringBuilder, pair: (StringBuilder, StringBuilder)): StringBuilder = {
      builder.append(pair._1)
      builder.append(':')
      builder.append(pair._2)
    }
    builder.append('{')
    map.headOption.foreach(pair => addPair(builder, pair))
    if (!map.isEmpty) {
      map.tail.foreach{pair =>
        builder.append(',')
        addPair(builder, pair)
      }
    }
    "{%s}".format(map.take(10).map(pair => "%s:%s".format(pair._1, pair._2)).mkString(","))
    builder.append('}')
  }
  def jsonObjectFieldsGenerator(depth: Int = 5): Gen[Seq[(JSONString, JSONValue)]] = listOfN(5, arbTuple2(Arbitrary(jsonStringGenerator), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary)
  def jsonObjectGenerator(depth: Int = 5): Gen[JSONObject] = arbImmutableMap(Arbitrary(jsonStringGenerator), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary.map{map =>
    JSONObject(map)
  }
  def valueGenerator(depth: Int = 10): Gen[StringBuilder] = {
    if (depth > 1) {
      oneOf(numberGenerator, stringGenerator, booleanGenerator, nothingGenerator, arrayGenerator(depth - 1), objectGenerator(depth - 1))
    } else {
      oneOf(numberGenerator, stringGenerator, booleanGenerator, nothingGenerator)
    }
  }
  def jsonValueGenerator(depth: Int = 10): Gen[JSONValue] = {
    if (depth > 1) {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator, jsonArrayGenerator(depth - 1), jsonObjectGenerator(depth - 1))
    } else {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)
    }
  }
  val arrayOrObjectGenerator = oneOf(arrayGenerator(), objectGenerator())
}