package com.github.seanparsons.jsonar

import org.specs2.{Specification, ScalaCheck}
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import JSONGenerators._

case class JSONObjectSpecification() extends Specification with ScalaCheck {
  def is =
    "JSONObject.apply" ^
      "Passing in a collection of fields" !
        forAll(jsonObjectFieldsGenerator()){fields =>
          val jsonObject = JSONObject(fields: _*)
          ("jsonObject = " + jsonObject) |: (jsonObject.fields === fields.toMap)
        }
}