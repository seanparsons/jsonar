package com.github.seanparsons.jsonar

case class ResultPairing(json: String, jValue: JSONValue)

object ValidResults {
  val resultPairings: Seq[ResultPairing] = Seq(
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
    ResultPairing("""[[{}],[{}]]""", JSONArray(Seq(JSONArray(Seq(JSONObject())), JSONArray(Seq(JSONObject())))))
  )
}