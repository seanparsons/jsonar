package com.github.seanparsons.jsonar

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import com.github.seanparsons.jsonar.JSONEscaping._
import org.specs2._
import org.specs2.specification._

case class JSONEscapingSpecification() extends Specification with ScalaCheck {
  def is = "Quoting" ^ 
    "Quoting and unquoting a string" !
      forAll(arbitrary[String]){text =>
        val quotedText = quote(text)
        ("quotedText = " + quotedText) |: {
          val unquotedText = unquote(quotedText)
          ("unquotedText = " + unquotedText) |: {
            text == unquotedText
          }
        }
      }
}