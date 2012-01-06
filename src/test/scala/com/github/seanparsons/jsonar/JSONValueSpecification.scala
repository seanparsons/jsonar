package com.github.seanparsons.jsonar

import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalatest.{ScalaTestScalazSupport, FeatureSpec}
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import JSONGenerators._

case class JSONValueSpecification() extends FeatureSpec
                                    with MustMatchers
                                    with Checkers
                                    with ScalaTestScalazSupport {
  feature("\\") {
    scenario("Any JSONValue that isn't a JSONObject returns None") {
      check(forAll(nonJSONObjectGenerator, arbitrary[String]){(jsonValue, key) =>
        jsonValue \ key ≟ none
      })
    }
    scenario("Any JSONObject with a single key present in the fields"){
      check(forAll(jsonStringGenerator, oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)){(key, value) =>
        val jsonObject = JSONObject((key -> value))
        jsonObject \ key.value ≟ value.some
      })
    }
    scenario("For comprehension drilling down") {
      val userMap: JSONObject = Map("1" -> "Sean", "2" -> "Ricky")
      val orderMap: JSONObject = Map("1" -> Seq("Item 1", "Item 2"), "2" -> Seq("Item 2", "Item 3"))
      val jsonObject: JSONObject = Map("users" -> userMap, "orders" -> orderMap)
      val nameAndOrders = for {
        username <- (jsonObject \ "users" \ "1").asJSONString
        userOrders <- (jsonObject \ "orders" \ "1").asJSONArray
      } yield (username, userOrders)
      nameAndOrders must equalByType((JSONString("Sean"), Seq("Item 1", "Item 2"): JSONArray).some)
    }
  } 
}