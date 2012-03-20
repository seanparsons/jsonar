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

case class ReadmeExampleSpecification() extends Specification with ScalaCheck {
  val complexJSON = """
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

  def is = "Readme" ^
    "Printer.print" ! {
      val json: String = Printer.print(JSONObject(JSONString("key") -> JSONString("value")))
      json ≟ """{"key":"value"}"""
    }
    "Parser.parse" ! {
      val parseResult: ValidationNEL[JSONError, JSONValue] = Parser.parse("[10]")
      parseResult ≟ Success(JSONArray(JSONInt(10)))
    }
    "For comprehension example" ! {
      val friendsOfUser = for {
        parsed <- Parser.parse(complexJSON)
        user <- (parsed \ "users" \ "1").asJSONString
        friendIDArray <- (parsed \ "friends" \ "1").asJSONArray
        friendIDs <- friendIDArray.collectElements{case JSONInt(value) => value}
      } yield (user.value, friendIDs)
      val expectedFriendsOfUser = ("Martin", Seq(BigInt(2), BigInt(3))).successNel[JSONError] 
      friendsOfUser ≟ expectedFriendsOfUser
    }
    "Complete for comprehension example" ! {
      val friendsOfUser = for {
        parsed <- Parser.parse(complexJSON)
        user <- parsed.search("users").search("1").asJSONString.asString
        friendIDs <- parsed.search("friends").search("1").asJSONArray.collectElements{case JSONInt(value) => value}
      } yield (user, friendIDs)
      val expectedFriendsOfUser = ("Martin", Seq(BigInt(2), BigInt(3))).successNel[JSONError] 
      friendsOfUser ≟ expectedFriendsOfUser
    }
}