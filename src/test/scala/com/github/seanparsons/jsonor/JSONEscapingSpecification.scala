package com.github.seanparsons.jsonor

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import com.github.seanparsons.jsonor.JSONEscaping._

case class JSONEscapingSpecification () extends FeatureSpec
                                        with MustMatchers
                                        with Checkers {
  feature("Quoting") {
    scenario("Quoting and unquoting a string") {
      check(forAll(arbitrary[String]){text =>
        val quotedText = quote(text)
        ("quotedText = " + quotedText) |: {
          val unquotedText = unquote(quotedText)
          ("unquotedText = " + unquotedText) |: {
            text == unquotedText
          }
        }
      })
    }
  }
}