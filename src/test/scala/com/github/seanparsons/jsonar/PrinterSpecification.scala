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


case class PrinterSpecification() extends Specification with DataTables with ScalaCheck {
  def printSpec = KnownResults.validResultPairings |> {(json, jsonValue) =>
    Printer.compact(jsonValue) must_== json
  }
  def prettyPrintedAndThenParsed = "Pretty printed and then parsed generates the same structure" !
    forAll(JSONGenerators.jsonValueGenerator().label("arrayOrObject")){jsonValue =>
      val prettyPrintedValue = Printer.pretty(jsonValue)
      ("prettyPrintedValue = " + prettyPrintedValue) |: {
        val parsedJSONValue = Parser.parse(prettyPrintedValue)
        ("parsedJSONValue = " + parsedJSONValue) |: jsonValue.successNel[JSONError] === parsedJSONValue
      }
    }

  def is = "print" ^ "valid results" ! printSpec ^ prettyPrintedAndThenParsed
}