package com.github.seanparsons.jsonar

import org.specs2._
import org.specs2.specification._
import org.specs2.matcher._

case class PrinterSpecification() extends Specification with DataTables with ScalaCheck {
  def printSpec = KnownResults.validResultPairings |> {(json, jsonValue) =>
    Printer.print(jsonValue) must_== json
  }
  def is = "print" ^ "valid results" ! printSpec 
}