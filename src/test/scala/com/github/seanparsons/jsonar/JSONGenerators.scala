package com.github.seanparsons.jsonar

import org.scalacheck.Gen._
import org.scalacheck.Prop
import org.scalacheck.Arbitrary.{arbBigDecimal => _,  _}
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalaz._
import Scalaz._
import scala.util.Random.shuffle
import java.math.MathContext._
import java.math.{BigDecimal => JavaBigDecimal}

object JSONGenerators {
  val maxJSONStructureDepth = 5

  def codePointStream(string: String): Stream[Int] = {
    // Try to remove anything that could be misconstrued as an escape character.
    val filteredString = JSONEscaping.quote(string.filter(_ != '\\'))
    def codePointStream(offset: Int): Stream[Int] = {
      if (offset > filteredString.length - 1) Stream.empty
      else {
        val codePoint = filteredString.codePointAt(offset)
        Stream.cons(codePoint, codePointStream(offset + Character.charCount(codePoint)))
      }
    }
    codePointStream(0)
  }
  def isValidUnicodeCodePoint(codePoint: Int): Boolean = {
    Character.isLetterOrDigit(codePoint) || Character.isWhitespace(codePoint) || Character.isISOControl(codePoint)
  }

  implicit val backportedArbBigDecimal: Arbitrary[BigDecimal] = {
    //val mcGen = oneOf(UNLIMITED, DECIMAL32, DECIMAL64, DECIMAL128)
    val mcGen = value(DECIMAL128)
    val bdGen = for {
      x <- arbBigInt.arbitrary
      mc <- mcGen
      limit <- value(if(mc == UNLIMITED) 0 else math.max(x.abs.toString.length - mc.getPrecision, 0))
      scale <- Gen.chooseNum((Int.MinValue / 2) + limit, Int.MaxValue / 2)
    } yield BigDecimal(x, scale, mc)
    Arbitrary(bdGen)
  }

  // Added filter to weed out the weirdo cases that seem to make BigDecimal assplode.
  val bigDecimalGenerator = arbitrary[BigDecimal].filter(bigDecimal => Validation.fromTryCatch(new JavaBigDecimal(bigDecimal.toString, UNLIMITED)).isSuccess)

  val intGenerator: Gen[StringBuilder] = arbitrary[Long].map(long => new StringBuilder().append(long))
  val jsonNumberGenerator: Gen[JSONNumber] = bigDecimalGenerator.map(number => JSONNumber(number))
  val doubleGenerator: Gen[StringBuilder] = arbitrary[Double].map(double => new StringBuilder().append(double))
  val numberGenerator: Gen[StringBuilder] = oneOf(intGenerator, doubleGenerator)
  val stringGenerator: Gen[StringBuilder] = arbitrary[String].map{string =>
    val codePoints = codePointStream(string).filter(isValidUnicodeCodePoint)
    val builder = codePoints.foldLeft(new java.lang.StringBuilder()){(builder, codePoint) =>
      if (codePoint <= 0xffff) {
        builder.append(codePoint.toChar)
      } else {
        builder.appendCodePoint(codePoint)
      }
    }
    new StringBuilder().append(builder)
  }
  val quotedStringGenerator: Gen[StringBuilder] = stringGenerator.map(builder => new StringBuilder().append("\"").append(builder).append("\""))
  val jsonStringGenerator: Gen[JSONString] = stringGenerator.map(stringBuilder => JSONString(stringBuilder.toString))
  val booleanGenerator: Gen[StringBuilder] = arbitrary[Boolean].map(boolean => new StringBuilder().append(if (boolean) "true" else "false"))
  val jsonBoolGenerator: Gen[JSONBool] = oneOf(JSONBoolTrue, JSONBoolFalse)
  val nothingGenerator: Gen[StringBuilder] = value(new StringBuilder().append("null"))
  val jsonNothingGenerator: Gen[JSONNull] = value(JSONNull)
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
  def jsonArrayItemsGenerator(depth: Int = maxJSONStructureDepth): Gen[Seq[JSONValue]] = listOfN(5, jsonValueGenerator(depth - 1))
  def jsonArrayGenerator(depth: Int = maxJSONStructureDepth): Gen[JSONArray] = jsonArrayItemsGenerator(depth).map{values => JSONArray(values)}
  def objectGenerator(depth: Int = maxJSONStructureDepth): Gen[StringBuilder] = arbImmutableMap(Arbitrary(quotedStringGenerator), Arbitrary(valueGenerator(depth - 1))).arbitrary.map{map =>
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
  def jsonObjectFieldsGenerator(depth: Int = maxJSONStructureDepth): Gen[Seq[(JSONString, JSONValue)]] = listOfN(5, arbTuple2(Arbitrary(jsonStringGenerator), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary)
  def jsonObjectGenerator(depth: Int = maxJSONStructureDepth): Gen[JSONObject] = arbImmutableMap(Arbitrary(jsonStringGenerator), Arbitrary(jsonValueGenerator(depth - 1))).arbitrary.map{map =>
    JSONObject(map)
  }
  def valueGenerator(depth: Int = maxJSONStructureDepth): Gen[StringBuilder] = {
    if (depth > 1) {
      oneOf(numberGenerator, quotedStringGenerator, booleanGenerator, nothingGenerator, arrayGenerator(depth - 1), objectGenerator(depth - 1))
    } else {
      oneOf(numberGenerator, quotedStringGenerator, booleanGenerator, nothingGenerator)
    }
  }
  val nonJSONObjectGenerator = oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator, jsonArrayGenerator())
  def jsonValueGenerator(depth: Int = maxJSONStructureDepth): Gen[JSONValue] = {
    if (depth > 1) {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator, jsonArrayGenerator(depth - 1), jsonObjectGenerator(depth - 1))
    } else {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)
    }
  }
  val arrayOrObjectGenerator = oneOf(arrayGenerator(), objectGenerator())

  def objectsOfObjectsGenerator(depth: Int = maxJSONStructureDepth): Gen[JSONValue] = {
    if (depth > 1) {
      listOfN(2, arbTuple2(Arbitrary(jsonStringGenerator), Arbitrary(objectsOfObjectsGenerator(depth - 1))).arbitrary).map(fields => JSONObject(fields.toMap))
    } else {
      oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)
    }
  }

  val arrayOrObjectAndPathGenerator: Gen[(Seq[String], JSONValue, JSONValue)] = objectsOfObjectsGenerator().map{jsonvalue =>
    def buildPath(currentPath: Seq[String], original: JSONValue, jsonValue: JSONValue): (Seq[String], JSONValue, JSONValue) = {
      jsonValue match {
        case jsonObject: JSONObject => {
          shuffle(jsonObject.fields.collect{case pair@ (innerString: JSONString, innerValue: JSONValue) => pair}.toList)
            .headOption
            .map{innerPair =>
              buildPath(currentPath :+ innerPair._1.value, original, innerPair._2)
            }
            .getOrElse((currentPath, original, jsonObject))
        }
        case other => (currentPath, original, other)
      }
    }
    buildPath(Seq(), jsonvalue, jsonvalue)
  }
}