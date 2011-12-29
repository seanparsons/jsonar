package com.github.seanparsons.jsonar

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import scalaz._
import Scalaz._

case class ParserSpecification() extends FeatureSpec
                                 with MustMatchers
                                 with Checkers {

  feature("parse") {
    ValidResults.resultPairings.foreach{resultPairing =>
      scenario("For the JSON %s, the parsed value is correct".format(resultPairing.json)) {
        Parser.parse(resultPairing.json) must equal(resultPairing.jValue.successNel[String])
      }
    }
    
    scenario("Parsed, printed and then parsed again generates the same structure") {
      check(forAllNoShrink(JSONGenerators.arrayOrObjectGenerator.map(_.toString)){json =>
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