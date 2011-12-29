package com.github.seanparsons.jsonar

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import JSONGenerators._

case class JSONObjectSpecification() extends FeatureSpec
                                      with MustMatchers
                                      with Checkers {
  feature("JSONObject.apply") {
    scenario("Passing in a collection of fields") {
      check(forAll(jsonObjectFieldsGenerator()){fields =>
        val jsonObject = JSONObject(fields: _*)
        jsonObject.fields == fields.toMap
      })
    }
  }
}