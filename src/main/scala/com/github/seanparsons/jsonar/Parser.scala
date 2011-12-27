package com.github.seanparsons.jsonar

import org.parboiled.scala._
import java.lang.String
import org.parboiled.errors.{ErrorUtils, ParsingException}
import scalaz._
import Scalaz._

object Parser extends Parser {

  case class ParserError(startIndex: Int, endIndex: Int, errorMessage: String)

  def jsonRule: Rule1[JSONValue] = rule { whitespaceRule ~ (jsonObjectRule | jsonArrayRule) ~ EOI }

  def jsonObjectRule: Rule1[JSONObject] = rule {
    "{ " ~ zeroOrMore(pairRule, separator = ", ") ~ "} " ~~> (fields => JSONObject(fields.toMap))
  }

  def pairRule: Rule1[(JSONString, JSONValue)] = rule {
    (jsonStringRule ~ ": " ~ valueRule) ~~> ((string, value) => (string, value))//((key: JSONString, _: String, value: JSONValue) => (key, value))
  }

  def valueRule: Rule1[JSONValue] = rule {
    jsonStringRule | jsonIntRule | jsonDecimalRule | jsonObjectRule | jsonArrayRule | jsonBoolTrueRule | jsonBoolFalseRule | jsonNullRule
  }

  def jsonStringRule: Rule1[JSONString] = rule {
    "\"" ~ zeroOrMore(characterRule) ~> (string => JSONString(JSONEscaping.unquote(string))) ~ "\" "
  }

  def jsonDecimalRule: Rule1[JSONDecimal] = rule {
    group(integerRule ~ optional(fractionRule ~ optional(exponentRule))) ~> ((matched) => JSONDecimal(BigDecimal(matched))) ~ whitespaceRule
  }

  def jsonIntRule: Rule1[JSONInt] = rule {
    integerRule ~> (value => JSONInt(BigInt(value))) ~ whitespaceRule
  }

  def jsonArrayRule: Rule1[JSONArray] = rule {
    "[ " ~ zeroOrMore(valueRule, separator = ", ") ~ "] " ~~> JSONArray.apply
  }

  def characterRule: Rule0 = rule { escapedCharRule | normalCharRule }

  def escapedCharRule: Rule0 = rule { "\\" ~ (anyOf("\"\\/bfnrt") | unicodeRule) }

  def normalCharRule: Rule0 = rule { !anyOf("\"\\") ~ ANY }

  def unicodeRule: Rule0 = rule { "u" ~ hexDigitRule ~ hexDigitRule ~ hexDigitRule ~ hexDigitRule }

  def integerRule: Rule0 = rule { optional("-") ~ (("1" - "9") ~ digitsRule | digitRule) }

  def digitsRule: Rule0 = rule { oneOrMore(digitRule) }

  def digitRule: Rule0 = rule { "0" - "9" }

  def hexDigitRule: Rule0 = rule { "0" - "9" | "a" - "f" | "A" - "Z" }

  def fractionRule: Rule0 = rule { "." ~ digitsRule }

  def exponentRule: Rule0 = rule { ignoreCase("e") ~ optional(anyOf("+-")) ~ digitsRule }

  def jsonBoolTrueRule: Rule1[JSONValue] = rule { "true " ~ push(JSONBoolTrue) }

  def jsonBoolFalseRule: Rule1[JSONValue] = rule { "false " ~ push(JSONBoolFalse) }

  def jsonNullRule: Rule1[JSONValue] = rule { "null " ~ push(JSONNull) }

  def whitespaceRule: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ whitespaceRule
    else
      str(string)

  def parse(json: String): ValidationNEL[ParserError, JSONValue] = {
    val parsingResult = ReportingParseRunner(jsonRule).run(json)
    parsingResult.result match {
      case Some(astRoot) => astRoot.successNel[ParserError]
      case None => parsingResult.parseErrors
                      .map(error => ParserError(error.getStartIndex, error.getEndIndex, error.getErrorMessage ?? "Parse error."))
                      .toNel
                      .getOrElse(NonEmptyList(ParserError(0, json.size, "No parse errors.")))
                      .fail[JSONValue]
    }
  }
}