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
  val doubleGenerator: Gen[StringBuilder] = arbitrary[Double].map(double => new StringBuilder().append(double))
  val numberGenerator: Gen[StringBuilder] = oneOf(intGenerator, doubleGenerator)
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
  val booleanGenerator: Gen[StringBuilder] = arbitrary[Boolean].map(boolean => new StringBuilder().append(if (boolean) "true" else "false"))
  def nothingGenerator: Gen[StringBuilder] = value(new StringBuilder().append("null"))
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
  def valueGenerator(depth: Int = 10): Gen[StringBuilder] = {
    if (depth > 1) {
      oneOf(numberGenerator, stringGenerator, booleanGenerator, nothingGenerator, arrayGenerator(depth - 1), objectGenerator(depth - 1))
    } else {
      oneOf(numberGenerator, stringGenerator, booleanGenerator, nothingGenerator)
    }
  }
  val arrayOrObjectGenerator = oneOf(arrayGenerator(), objectGenerator())
}