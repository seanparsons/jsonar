package com.github.seanparsons.jsonar

import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz.{none => scalazNone, _}
import JSONGenerators._
import org.scalacheck.{Prop, Gen}
import org.specs2._
import org.specs2.specification._
import org.specs2.matcher.DataTables
import com.github.seanparsons.jsonar._

case class JSONValueSpecification() extends Specification with ScalaCheck with DataTables {
  val minimalParams = set(minTestsOk -> 3)

  case class GeneratorAndConversionChecks[T <: JSONValue](method: String, gen: Gen[T], validCheck: (T) => Prop, invalidCheck: (JSONValue) => Prop)(implicit manifest: Manifest[T]) {
    def clazz = manifest.erasure
    def performValidCheck(): Prop = forAll(gen){jsonValue => validCheck(jsonValue)}
    def performInvalidCheck(gen: Gen[JSONValue]): Prop = forAll(gen){jsonValue => invalidCheck(jsonValue)}
  }
  val jsonValueGenerators = Set[GeneratorAndConversionChecks[_ <: JSONValue]](
    GeneratorAndConversionChecks[JSONNull]("as[JSONNull]", jsonNothingGenerator, (jsonNull) => jsonNull.as[JSONNull] === jsonNull.success, (jsonValue) => jsonValue.as[JSONNull].isFailure),
    GeneratorAndConversionChecks[JSONNumber]("as[JSONNumber]", jsonNumberGenerator, (jsonNumber) => jsonNumber.as[JSONNumber] === jsonNumber.success, (jsonValue) => jsonValue.as[JSONNumber].isFailure),
    GeneratorAndConversionChecks[JSONString]("asJSONString", jsonStringGenerator, (jsonString) => jsonString.as[JSONString] === jsonString.success, (jsonValue) => jsonValue.as[JSONString].isFailure),
    GeneratorAndConversionChecks[JSONBool]("as[JSONBool]", jsonBoolGenerator, (jsonBool) => jsonBool.as[JSONBool] === jsonBool.success, (jsonValue) => jsonValue.as[JSONBool].isFailure),
    GeneratorAndConversionChecks[JSONArray]("as[JSONArray]", jsonArrayGenerator(), (jsonArray) => jsonArray.as[JSONArray] === jsonArray.success, (jsonValue) => jsonValue.as[JSONArray].isFailure),
    GeneratorAndConversionChecks[JSONObject]("as[JSONObject]", jsonObjectGenerator(), (jsonObject) => jsonObject.as[JSONObject] === jsonObject.success, (jsonValue) => jsonValue.as[JSONObject].isFailure)
  )

  case class OptionalConversionChecks[T <: JSONValue](method: String, gen: Gen[T], validCheck: (T) => Prop, invalidCheck: (JSONValue) => Prop, nullCheck: (JSONNull) => Prop)(implicit manifest: Manifest[T]) {
    def clazz = manifest.erasure
    def performValidCheck(): Prop = forAll(gen){jsonValue => validCheck(jsonValue)}
    def performInvalidCheck(gen: Gen[JSONValue]): Prop = forAll(gen){jsonValue => invalidCheck(jsonValue)}
    def performNullCheck(): Prop = forAll(jsonNothingGenerator){jsonNull => nullCheck(jsonNull)}
  }
  val optionalConversionChecks = Set[OptionalConversionChecks[_ <: JSONValue]](
    OptionalConversionChecks[JSONNumber]("asOptional[JSONNumber]", jsonNumberGenerator, (jsonNumber) => jsonNumber.asOptional[JSONNumber] === jsonNumber.some.success, (jsonValue) => jsonValue.asOptional[JSONNumber].isFailure, (jsonNull) => jsonNull.asOptional[JSONNumber] === scalazNone[JSONNumber].successNel[JSONError]),
    OptionalConversionChecks[JSONString]("asOptional[JSONString]", jsonStringGenerator, (jsonString) => jsonString.asOptional[JSONString] === jsonString.some.success, (jsonValue) => jsonValue.asOptional[JSONString].isFailure, (jsonNull) => jsonNull.asOptional[JSONString] === scalazNone[JSONString].successNel[JSONError]),
    OptionalConversionChecks[JSONBool]("asOptional[JSONBool]", jsonBoolGenerator, (jsonBool) => jsonBool.asOptional[JSONBool] === jsonBool.some.success, (jsonValue) => jsonValue.asOptional[JSONBool].isFailure, (jsonNull) => jsonNull.asOptional[JSONBool] === scalazNone[JSONBool].successNel[JSONError]),
    OptionalConversionChecks[JSONArray]("asOptional[JSONArray]", jsonArrayGenerator(), (jsonArray) => jsonArray.asOptional[JSONArray] === jsonArray.some.success, (jsonValue) => jsonValue.asOptional[JSONArray].isFailure, (jsonNull) => jsonNull.asOptional[JSONArray] === scalazNone[JSONArray].successNel[JSONError]),
    OptionalConversionChecks[JSONObject]("asOptional[JSONObject]", jsonObjectGenerator(), (jsonObject) => jsonObject.asOptional[JSONObject] === jsonObject.some.success, (jsonValue) => jsonValue.asOptional[JSONObject].isFailure, (jsonNull) => jsonNull.asOptional[JSONObject] === scalazNone[JSONObject].successNel[JSONError])
  )
  
  def doubleSlashSpec = "/" ^
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
        usersJSONObject <- users.as[JSONObject]
        usernameJSONValue <- usersJSONObject / "1"
        username <- usernameJSONValue.as[JSONString]
        orders <- jsonObject / "orders"
        ordersJSONObject <- orders.as[JSONObject]
        userOrders <- ordersJSONObject / "1"
        ordersJSONArray <- userOrders.as[JSONArray]
      } yield (username, userOrders)
      nameAndOrders must_== expectedResult
    } ^ end
  
  def generatorAndConversionSpec = jsonValueGenerators.map{generatorAndConversionCheck =>
    generatorAndConversionCheck.method ^ {
      "For a " + generatorAndConversionCheck.clazz.getSimpleName ! check(generatorAndConversionCheck.performValidCheck())(minimalParams) ^
      ((jsonValueGenerators - generatorAndConversionCheck)
        .map{otherGeneratorAndConversionCheck: GeneratorAndConversionChecks[_ <: JSONValue] =>
          ("Called on a " + otherGeneratorAndConversionCheck.clazz.getSimpleName) ! {
            check(generatorAndConversionCheck.performInvalidCheck(otherGeneratorAndConversionCheck.gen))(minimalParams)
          }: Fragments
        }.reduceLeft(_ ^ _))
      } ^ end
    }.reduceLeft(_ ^ _)
  def optionalConversionSpec = optionalConversionChecks.map{optionalCheck =>
    optionalCheck.method ^ {
      "For a " + optionalCheck.clazz.getSimpleName ! check(optionalCheck.performValidCheck())(minimalParams) ^
      "For a null value" ! check(optionalCheck.performNullCheck())(minimalParams) ^
      ((optionalConversionChecks - optionalCheck)
        .map{otherOptionalConversionsChecks: OptionalConversionChecks[_ <: JSONValue] =>
          ("Called on a " + otherOptionalConversionsChecks.clazz.getSimpleName) ! {
            check(optionalCheck.performInvalidCheck(otherOptionalConversionsChecks.gen))(minimalParams)
          }: Fragments
        }.reduceLeft(_ ^ _))
      } ^ end
    }.reduceLeft(_ ^ _)
  
  def is = args.report(failtrace = true) ^ doubleSlashSpec ^ generatorAndConversionSpec ^ optionalConversionSpec
  
}