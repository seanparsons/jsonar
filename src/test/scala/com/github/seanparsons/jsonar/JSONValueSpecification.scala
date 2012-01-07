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
import org.scalacheck.{Prop, Gen}

case class JSONValueSpecification() extends FeatureSpec
                                    with MustMatchers
                                    with Checkers
                                    with ScalaTestScalazSupport {

  case class GeneratorAndConversionChecks[T <: JSONValue](method: String, gen: Gen[T], validCheck: (T) => Prop, invalidCheck: (JSONValue) => Prop) {
    def performValidCheck(): Prop = forAll(gen){jsonValue => validCheck(jsonValue)}
    def performInvalidCheck(gen: Gen[JSONValue]): Prop = forAll(gen){jsonValue => invalidCheck(jsonValue)}
  }
  val jsonValueGenerators = Map[Class[_], GeneratorAndConversionChecks[JSONValue]](
    classOf[JSONNull] -> new GeneratorAndConversionChecks[JSONNull]("asJSONNull", jsonNothingGenerator, (jsonNull) => jsonNull.asJSONNull ≟ jsonNull.success, (jsonValue) => jsonValue.asJSONNull.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONInt] -> new GeneratorAndConversionChecks[JSONInt]("asJSONInt", jsonIntGenerator, (jsonInt) => jsonInt.asJSONInt ≟ jsonInt.success, (jsonValue) => jsonValue.asJSONInt.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONDecimal] -> new GeneratorAndConversionChecks[JSONDecimal]("asJSONDecimal", jsonDecimalGenerator, (jsonDecimal) => jsonDecimal.asJSONDecimal ≟ jsonDecimal.success, (jsonValue) => jsonValue.asJSONDecimal.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONString] -> new GeneratorAndConversionChecks[JSONString]("asJSONString", jsonStringGenerator, (jsonString) => jsonString.asJSONString ≟ jsonString.success, (jsonValue) => jsonValue.asJSONString.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONBool] -> new GeneratorAndConversionChecks[JSONBool]("asJSONBool", jsonBoolGenerator, (jsonBool) => jsonBool.asJSONBool ≟ jsonBool.success, (jsonValue) => jsonValue.asJSONBool.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONArray] -> new GeneratorAndConversionChecks[JSONArray]("asJSONArray", jsonArrayGenerator(), (jsonArray) => jsonArray.asJSONArray ≟ jsonArray.success, (jsonValue) => jsonValue.asJSONArray.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]],
    classOf[JSONObject] -> new GeneratorAndConversionChecks[JSONObject]("asJSONObject", jsonObjectGenerator(), (jsonObject) => jsonObject.asJSONObject ≟ jsonObject.success, (jsonValue) => jsonValue.asJSONObject.isFailure).asInstanceOf[GeneratorAndConversionChecks[JSONValue]]
  )

  feature("\\") {
    scenario("Any JSONValue that isn't a JSONObject returns None") {
      check(forAll(nonJSONObjectGenerator, arbitrary[String]){(jsonValue, key) =>
        val lookupResult = jsonValue \ key
        ("lookupResult = " + lookupResult) |: lookupResult ≟ "Could not find subelement \"%s\" in %s.".format(key, jsonValue).fail
      })
    }
    scenario("Any JSONObject with a single key present in the fields"){
      check(forAll(jsonStringGenerator, oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)){(key, value) =>
        val jsonObject = JSONObject((key -> value))
        jsonObject \ key.value ≟ value.success
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
      nameAndOrders must equalByType((JSONString("Sean"), Seq("Item 1", "Item 2"): JSONArray).success[String])
    }
  }
  jsonValueGenerators.keySet.foreach{clazz =>
    val generatorAndConversionChecks = jsonValueGenerators(clazz)
    feature(generatorAndConversionChecks.method) {
      scenario("For a " + clazz.getSimpleName) {
        check(generatorAndConversionChecks.performValidCheck())
      }
      scenario("For any instance that isn't of type " + clazz.getSimpleName) {
        val gen: Gen[JSONValue] = frequency((jsonValueGenerators - clazz).values.map(generator => (1, generator.gen: Gen[JSONValue])).toSeq: _*)
        check(generatorAndConversionChecks.performInvalidCheck(gen))
      }
    }
  }
}