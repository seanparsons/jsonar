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
        ("jsonObject = " + jsonObject) |: jsonObject.fields == fields.toMap
      })
    }
  }
  feature("map") {
    scenario("Identity transform returns identical instance") {
      check(forAll(jsonObjectGenerator()){jsonObject =>
        val mapped = jsonObject.map(identity)
        ("mapped = " + mapped) |: jsonObject == mapped
      })
    }
  }
}