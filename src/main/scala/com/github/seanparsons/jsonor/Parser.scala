package com.github.seanparsons.jsonor

import org.parboiled.scala._
import java.lang.String
import org.parboiled.errors.{ErrorUtils, ParsingException}
import scalaz._
import Scalaz._

object Parser extends Parser {

  def jsonRule: Rule1[JSONValue] = rule { whitespaceRule ~ (jsonObjectRule | jsonArrayRule) ~ EOI }

  def jsonObjectRule: Rule1[JSONObject] = rule {
    "{ " ~ zeroOrMore(pairRule, separator = ", ") ~ "} " ~~> (fields => JSONObject(fields.toMap))
  }

  def pairRule: Rule1[(JSONString, JSONValue)] = rule {
    (jsonStringRule ~ ": " ~ valueRule) ~~> ((string, value) => (string, value))//((key: JSONString, _: String, value: JSONValue) => (key, value))
  }

  def valueRule: Rule1[JSONValue] = rule {
    jsonStringRule | jsonDecimalRule | jsonIntRule | jsonObjectRule | jsonArrayRule | jsonBoolTrueRule | jsonBoolFalseRule | jsonNullRule
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

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ whitespaceRule
    else
      str(string)

  /**
   * The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
   */
  def parse(json: String): ValidationNEL[String, JSONValue] = {
    val parsingResult = ReportingParseRunner(jsonRule).run(json)
    parsingResult.result match {
      case Some(astRoot) => astRoot.successNel[String]
      case None => parsingResult.parseErrors
                      .map{error =>
                        val start = error.getStartIndex
                        val end = error.getEndIndex
                        val length = end - start
                        val escapedText = json.drop(start - 5).take(length + 10).map(char => "\\u%04x".format(char: Int))
                        "%s-%s:%s:%s".format(error.getStartIndex, error.getEndIndex, error.getErrorMessage ?? "Parse error.", escapedText)
                      }
                      .toNel
                      .getOrElse(NonEmptyList("No parse errors."))
                      .fail[JSONValue]
    }
  }
}