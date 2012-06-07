package com.github.seanparsons.jsonar

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import JSONGenerators._

case class JSONObjectSpecification() extends Specification with ScalaCheck {
  def is = args.report(failtrace = true) ^
    "JSONObject.apply" ^
      "Passing in a collection of fields" !
        forAll(jsonObjectFieldsGenerator()){fields =>
          val jsonObject = JSONObject(fields: _*)
          // TODO: Reinstate the use of ===.
          ("jsonObject = " + jsonObject) |: (jsonObject.fields == fields.toMap)
        } ^
      "Searching through multiple levels will find the correct value" !
        forAll(arrayOrObjectAndPathGenerator){case (path, original, innerValue) =>
          val searchedForValue = path.tail.foldLeft(original / path.head)((value, pathElement) => value / pathElement)
          ("searchedForValue = " + searchedForValue) |: (searchedForValue === innerValue.successNel[JSONError])
        } ^ end
}