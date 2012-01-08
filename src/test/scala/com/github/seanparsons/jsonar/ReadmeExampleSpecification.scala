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

case class ReadmeExampleSpecification() extends FeatureSpec
                                        with MustMatchers
                                        with Checkers
                                        with ScalaTestScalazSupport {
  feature("Readme") {
    scenario("Printer.print") {
      val json: String = Printer.print(JSONObject(JSONString("key") -> JSONString("value")))
      json must equal("""{"key":"value"}""")
    }
    scenario("Parser.parse") {
      val parseResult: ValidationNEL[String, JSONValue] = Parser.parse("[10]")
      parseResult must equal(Success(JSONArray(JSONInt(10))))
    }
    scenario("For comprehensions example") {
      val json = """
        {
          "users":
          {
            "1":"Martin",
            "2":"Rich",
            "3":"James"
          },
          "friends":
          {
            "1":[2,3],
            "2":[1,3],
            "3":[1]
          }
        }
      """

      val friendsOfUser = for {
        parsed <- Parser.parse(json)
        user <- (parsed \ "users" \ "1").asJSONString
        friendIDArray <- (parsed \ "friends" \ "1").asJSONArray
        friendIDs <- friendIDArray.collectElements{case JSONInt(value) => value}
      } yield (user.value, friendIDs)
      friendsOfUser must equal(Success(("Martin", Vector(2,3))))
    }
  }
}