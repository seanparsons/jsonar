package com.github.seanparsons.jsonor

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import LiftJSONGenerators._
import scalaz._
import Scalaz._
import net.liftweb.json.{Printer => LiftPrinter, _}
import net.liftweb.json.JsonParser.ParseException

case class ParserSpecification() extends FeatureSpec
                                 with MustMatchers
                                 with Checkers {

  def parseWithLift(json: String): ValidationNEL[String, JValue] = {
    try {
      stabiliseJValue(parse(json)).successNel
    } catch {
      case parseException: ParseException => {
        if (parseException.getCause == null) {
          parseException.getMessage.failNel
        } else {
          "%s: %s".format(parseException.getMessage, parseException.getCause.getMessage).failNel
        }
      }
    }
  }

  feature("parse") {
    /*
    scenario("Compact JSON parsed and then printed back out parses the same") {
      check(forAllNoShrink(arrayOrObjectGenerator){jValue =>
        val json = jValue |> render |> compact
        ("json = " + json) |: {
          val parsedValidation = Parser.parse(json)
          ("parsedValidation = " + parsedValidation) |: {
            val printedJSONValidation = parsedValidation.map(jsonValue => Printer.toString(jsonValue))
            ("printedJSONValidation = " + printedJSONValidation) |: {
              val parsedJValue = printedJSONValidation.flatMap(printedJSON => parseWithLift(printedJSON))
              ("parsedJValue = " + parsedJValue) |: {
                parsedJValue == jValue.successNel[String]
              }
            }
          }
        }
      })
    }
    */

    scenario("Parsed, printed and then parsed again generates the same structure") {
      check(forAllNoShrink(JSONGenerators.arrayOrObjectGenerator.map(_.toString)){json =>
        val firstParsed = Parser.parse(json)
        ("firstParsed = " + firstParsed) |: {
          val printedJSON = firstParsed.map(jsonValue => Printer.toString(jsonValue))
          ("printedJSON = " + printedJSON) |: {
            val secondParsed = printedJSON.flatMap(secondJSON => Parser.parse(secondJSON))
            ("secondParsed = " + secondParsed) |: all {
              firstParsed == secondParsed
              firstParsed.isSuccess == true
            }
          }
        }
      })
    }
  }
}