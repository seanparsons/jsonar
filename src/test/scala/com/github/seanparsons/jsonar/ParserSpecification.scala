package com.github.seanparsons.jsonar

import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import org.specs2._
import org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

case class ParserSpecification() extends Specification with DataTables with ScalaCheck {
  def validResultsSpec = KnownResults.validResultPairings |> {(json, expectedJSONValue) =>
    val actualParseResult = Parser.parse(json)
    actualParseResult must_== Success[String, JSONValue](expectedJSONValue).liftFailNel
  }
  def invalidResultsSpec = KnownResults.parseFailures |> {(json, parseResult) =>
    val actualParseResult = Parser.parse(json)
    actualParseResult ≟ parseResult
  }
  def parsedPrintedThenParsedAgainSpec = "Parsed, printed and then parsed again generates the same structure" !
    forAll(JSONGenerators.arrayOrObjectGenerator.map(_.toString).label("arrayOrObject")){json =>
      val firstParsed = Parser.parse(json)
      ("firstParsed = " + firstParsed) |: {
        val printedJSON = firstParsed.map(jsonValue => Printer.print(jsonValue))
        ("printedJSON = " + printedJSON) |: {
          val secondParsed = printedJSON.flatMap(secondJSON => Parser.parse(secondJSON))
          ("secondParsed = " + secondParsed) |: (firstParsed must_== secondParsed)
        }
      }
    } 
  def is = "parse" ^ "valid results" ! validResultsSpec ^ "invalid results" ! invalidResultsSpec ^ parsedPrintedThenParsedAgainSpec
    
}