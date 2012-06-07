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
      val json: String = Printer.compact(JSONObject(JSONString("key") -> JSONString("value")))
      json === """{"key":"value"}"""
    }
    "Parser.parse" ! {
      val parseResult: ValidationNEL[JSONError, JSONValue] = Parser.parse("[10]")
      parseResult === Success(JSONArray(JSONNumber(10)))
    }
    "For comprehension example" ! {
      val friendsOfUser = for {
        parsed <- Parser.parse(complexJSON)
        parsedJSONObject <- parsed.as[JSONObject]
        users <- parsedJSONObject / "users"
        usersJSONObject <- users.as[JSONObject]
        user <- usersJSONObject / "1"
        userName <- user.as[String]
        friends <- parsedJSONObject / "friends"
        friendsJSONObject <- friends.as[JSONObject]
        friendIDs <- friendsJSONObject / "1"
        friendIDArray <- friendIDs.as[JSONArray]
      } yield (userName, friendIDArray.elements.collect{case JSONNumber(int) => int}.toList)
      val expectedFriendsOfUser = ("Martin", List(BigDecimal(2), BigDecimal(3))).successNel[JSONError]
      friendsOfUser === expectedFriendsOfUser
    }
    "Complete for comprehension example" ! {
      val friendsOfUser = for {
        parsed <- Parser.parse(complexJSON)
        user <- parsed.as[JSONObject].flatMap(_.search("users")).flatMap(_.as[JSONObject]).flatMap(_.search("1")).flatMap(_.as[String])
        friendIDs <- parsed.as[JSONObject].flatMap(_.search("friends")).flatMap(_.as[JSONObject]).flatMap(_.search("1")).flatMap(_.as[JSONArray]).map(_.elements.collect{case JSONNumber(value) => value})
      } yield (user, friendIDs.toList)
      val expectedFriendsOfUser = ("Martin", List(BigDecimal(2), BigDecimal(3))).successNel[JSONError]
      friendsOfUser === expectedFriendsOfUser
    }
}