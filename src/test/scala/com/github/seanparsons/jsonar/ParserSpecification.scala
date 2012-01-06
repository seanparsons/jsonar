package com.github.seanparsons.jsonar

import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import scalaz._
import Scalaz._
import org.scalacheck._
import org.scalacheck.Shrink._
import org.scalatest.{ScalaTestScalazSupport, FeatureSpec}

case class ParserSpecification() extends FeatureSpec
                                 with MustMatchers
                                 with Checkers
                                 with ScalaTestScalazSupport {
  feature("parse") {
    KnownResults.expectedParseResults.foreach{expectedParseResult =>
      scenario("For the JSON %s, the parse result is correct".format(expectedParseResult.json)) {
        val actualParseResult = Parser.parse(expectedParseResult.json)
        actualParseResult must equalByType(expectedParseResult.result)
      }
    }
    
    scenario("Parsed, printed and then parsed again generates the same structure") {
      check(forAll(JSONGenerators.arrayOrObjectGenerator.map(_.toString).label("arrayOrObject")){json =>
        val firstParsed = Parser.parse(json)
        ("firstParsed = " + firstParsed) |: {
          val printedJSON = firstParsed.map(jsonValue => Printer.print(jsonValue))
          ("printedJSON = " + printedJSON) |: {
            val secondParsed = printedJSON.flatMap(secondJSON => Parser.parse(secondJSON))
            ("secondParsed = " + secondParsed) |: firstParsed ≟ secondParsed
          }
        }
      })
    }
  }
}