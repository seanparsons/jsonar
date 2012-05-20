package com.github.seanparsons.jsonar

import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import JSONGenerators._
import org.scalacheck.{Prop, Gen}
import org.specs2._
import org.specs2.specification._
import org.specs2.matcher.DataTables

case class JSONValueSpecification() extends Specification with ScalaCheck with DataTables {
  
  case class GeneratorAndConversionChecks[T <: JSONValue](method: String, clazz: Class[T], gen: Gen[T], validCheck: (T) => Prop, invalidCheck: (JSONValue) => Prop) {
    def performValidCheck(): Prop = forAll(gen){jsonValue => validCheck(jsonValue)}
    def performInvalidCheck(gen: Gen[JSONValue]): Prop = forAll(gen){jsonValue => invalidCheck(jsonValue)}
  }
  val jsonValueGenerators = Set[GeneratorAndConversionChecks[_ <: JSONValue]](
    new GeneratorAndConversionChecks[JSONNull]("asJSONNull", classOf[JSONNull], jsonNothingGenerator, (jsonNull) => jsonNull.asJSONNull === jsonNull.success, (jsonValue) => jsonValue.asJSONNull.isFailure),
    new GeneratorAndConversionChecks[JSONNumber]("asJSONNumber", classOf[JSONNumber], jsonNumberGenerator, (jsonNumber) => jsonNumber.asJSONNumber === jsonNumber.success, (jsonValue) => jsonValue.asJSONNumber.isFailure),
    new GeneratorAndConversionChecks[JSONString]("asJSONString", classOf[JSONString], jsonStringGenerator, (jsonString) => jsonString.asJSONString === jsonString.success, (jsonValue) => jsonValue.asJSONString.isFailure),
    new GeneratorAndConversionChecks[JSONBool]("asJSONBool", classOf[JSONBool], jsonBoolGenerator, (jsonBool) => jsonBool.asJSONBool === jsonBool.success, (jsonValue) => jsonValue.asJSONBool.isFailure),
    new GeneratorAndConversionChecks[JSONArray]("asJSONArray", classOf[JSONArray], jsonArrayGenerator(), (jsonArray) => jsonArray.asJSONArray === jsonArray.success, (jsonValue) => jsonValue.asJSONArray.isFailure),
    new GeneratorAndConversionChecks[JSONObject]("asJSONObject", classOf[JSONObject], jsonObjectGenerator(), (jsonObject) => jsonObject.asJSONObject === jsonObject.success, (jsonValue) => jsonValue.asJSONObject.isFailure)
  )
  
  def doubleSlashSpec = "\\" ^
    "Any JSONObject with a single key present in the fields" !
      forAll(jsonStringGenerator, Gen.oneOf(jsonNumberGenerator, jsonStringGenerator, jsonBoolGenerator, jsonNothingGenerator)){(key, value) =>
        val jsonObject = JSONObject(key -> value)
        jsonObject / key.value === value.success
      } ^
    "For comprehension drilling down" ! {
      val userMap: JSONObject = JSONObject(Map(JSONString("1") -> JSONString("Sean"), JSONString("2") -> JSONString("Ricky")))
      val orderMap: JSONObject = JSONObject(Map(JSONString("1") -> JSONArray(JSONString("Item 1"), JSONString("Item 2")), JSONString("2") -> JSONArray(JSONString("Item 2"), JSONString("Item 3"))))
      val jsonObject: JSONObject = JSONObject(Map(JSONString("users") -> userMap, JSONString("orders") -> orderMap))
      val expectedResult: Validation[String, (JSONString, JSONArray)] = (JSONString("Sean"), JSONArray(JSONString("Item 1"), JSONString("Item 2"))).success[String]
      val nameAndOrders = for {
        users <- jsonObject / "users"
        usersJSONObject <- users.asJSONObject
        usernameJSONValue <- usersJSONObject / "1"
        username <- usernameJSONValue.asJSONString
        orders <- jsonObject / "orders"
        ordersJSONObject <- orders.asJSONObject
        userOrders <- ordersJSONObject / "1"
        ordersJSONArray <- userOrders.asJSONArray
      } yield (username, userOrders)
      nameAndOrders must_== expectedResult
    } ^ end
  
  def generatorAndConversionSpec = jsonValueGenerators.map{generatorAndConversionCheck =>
    generatorAndConversionCheck.method ^ {
      "For a " + generatorAndConversionCheck.clazz.getSimpleName ! generatorAndConversionCheck.performValidCheck() ^
      "For any instance that isn't of type " + generatorAndConversionCheck.clazz.getSimpleName ! {
        val gen: Gen[JSONValue] = frequency((jsonValueGenerators - generatorAndConversionCheck).map(generator => (1, generator.gen: Gen[JSONValue])).toSeq: _*)
        check(generatorAndConversionCheck.performInvalidCheck(gen))
      } ^ end
    }
  }.reduceLeft(_ ^ _)
  
  def is = args.report(failtrace = true) ^ doubleSlashSpec ^ generatorAndConversionSpec
  
}