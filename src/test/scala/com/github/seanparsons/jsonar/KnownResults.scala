package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._

case class ResultPairing(json: String, jValue: JSONValue)
case class ExpectedParseResult(json: String, result: ValidationNEL[String, JSONValue]) {
  def this(resultPairing: ResultPairing) = this(resultPairing.json, resultPairing.jValue.successNel)
}

object KnownResults {
  val validResultPairings: Seq[ResultPairing] = Seq(
    ResultPairing("""[]""", JSONArray()),
    ResultPairing("""{}""", JSONObject()),
    ResultPairing("""[10]""", JSONArray(Seq(JSONInt(10)))),
    ResultPairing("""{"number":20}""", JSONObject(Map(JSONString("number") -> JSONInt(20)))),
    ResultPairing("""{"firstKey":100,"secondKey":"secondValue"}""", new JSONObject(JSONString("firstKey") -> JSONInt(100), JSONString("secondKey") -> JSONString("secondValue"))),
    ResultPairing("""[100,"secondValue"]""", JSONArray(Seq(JSONInt(100), JSONString("secondValue")))),
    ResultPairing("""[[]]""", JSONArray(Seq(JSONArray()))),
    ResultPairing("""[[[]]]""", JSONArray(Seq(JSONArray(Seq(JSONArray()))))),
    ResultPairing("""[[],[]]""", JSONArray(Seq(JSONArray(), JSONArray()))),
    ResultPairing("""[{},{}]""", JSONArray(Seq(JSONObject(), JSONObject()))),
    ResultPairing("""[[{}],[{}]]""", JSONArray(Seq(JSONArray(Seq(JSONObject())), JSONArray(Seq(JSONObject()))))),
    ResultPairing(""""\t"""", JSONString("\t")),
    ResultPairing(""""\b"""", JSONString("\b")),
    ResultPairing(""""\f"""", JSONString("\f")),
    ResultPairing(""""\n"""", JSONString("\n")),
    ResultPairing(""""\r"""", JSONString("\r")),
    ResultPairing(""""\\"""", JSONString("\\")),
    ResultPairing(""""\/"""", JSONString("/")),
    ResultPairing(""""\""""", JSONString("\""))
  )
  val parseFailures: Seq[ExpectedParseResult] = Seq(
    ExpectedParseResult("""[][]""", "JSON contains invalid suffix content: []".failNel),
    ExpectedParseResult("""{}{}""", "JSON contains invalid suffix content: {}".failNel),
    ExpectedParseResult("\"\"\"\"", "JSON contains invalid suffix content: \"\"".failNel),
    ExpectedParseResult("\"test", "Expected string bounds but found: ".failNel),
    ExpectedParseResult("[7,,]", "Unexpected content found: ,]".failNel),
    ExpectedParseResult("""{"firstKey":100,"secondKey":}""", "Unexpected content found: }".failNel),
    ExpectedParseResult("""{"firstKey":}""", "Unexpected content found: }".failNel),
    ExpectedParseResult("""{"firstKey"}""", "Expected field separator token but found: }".failNel),
    ExpectedParseResult("""[[}]""", "Unexpected content found: }]".failNel)
  )
  val expectedParseResults = validResultPairings.map(pairing => new ExpectedParseResult(pairing)) ++ parseFailures
}