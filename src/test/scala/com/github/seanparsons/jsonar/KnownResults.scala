package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._
import org.specs2.matcher.DataTables

object KnownResults extends DataTables {
  def validResultPairings = 
    "JSON"                                            | "JSONValue"                                                                                                           |
    """[]"""                                          ! JSONArray()                                                                                                           |
    """{}"""                                          ! JSONObject()                                                                                                          |                                              
    """[10]"""                                        ! JSONArray(Seq(JSONInt(10)))                                                                                           |
    """{"number":20}"""                               ! JSONObject(Map(JSONString("number") -> JSONInt(20)))                                                                  |
    """{"firstKey":100,"secondKey":"secondValue"}"""  ! new JSONObject(JSONString("firstKey") -> JSONInt(100), JSONString("secondKey") -> JSONString("secondValue"))          |
    """[100,"secondValue"]"""                         ! JSONArray(Seq(JSONInt(100), JSONString("secondValue")))                                                               |
    """[[]]"""                                        ! JSONArray(Seq(JSONArray()))                                                                                           |
    """[[[]]]"""                                      ! JSONArray(Seq(JSONArray(Seq(JSONArray()))))                                                                           |
    """[[],[]]"""                                     ! JSONArray(Seq(JSONArray(), JSONArray()))                                                                              |
    """[{},{}]"""                                     ! JSONArray(Seq(JSONObject(), JSONObject()))                                                                            |
    """[[{}],[{}]]"""                                 ! JSONArray(Seq(JSONArray(Seq(JSONObject())), JSONArray(Seq(JSONObject()))))                                            |
    """"\t""""                                        ! JSONString("\t")                                                                                                      |
    """"\b""""                                        ! JSONString("\b")                                                                                                      |
    """"\f""""                                        ! JSONString("\f")                                                                                                      |
    """"\n""""                                        ! JSONString("\n")                                                                                                      |
    """"\r""""                                        ! JSONString("\r")                                                                                                      |
    """"\\""""                                        ! JSONString("\\")                                                                                                      |
    """"\/""""                                        ! JSONString("/")                                                                                                       |
    """"\"""""                                        ! JSONString("\"")                                                                                                      |
    "158699798998941697"                              ! JSONInt(BigInt(158699798998941697l))
    
  def parseFailures = 
    "JSON"                                            | "parse result"                                                                                                        |
    """[][]"""                                        ! "JSON contains invalid suffix content: []".failNel[JSONValue]                                                         |
    """{}{}"""                                        ! "JSON contains invalid suffix content: {}".failNel[JSONValue]                                                         |
    "\"\"\"\""                                        ! "JSON contains invalid suffix content: \"\"".failNel[JSONValue]                                                       |
    "\"test"                                          ! "Expected string bounds but found: ".failNel[JSONValue]                                                               |
    "[7,,]"                                           ! "Unexpected content found: ,]".failNel[JSONValue]                                                                     |
    """{"firstKey":100,"secondKey":}"""               ! "Unexpected content found: }".failNel[JSONValue]                                                                      |
    """{"firstKey":}"""                               ! "Unexpected content found: }".failNel[JSONValue]                                                                      |
    """{"firstKey"}"""                                ! "Expected field separator token but found: }".failNel[JSONValue]                                                      |
    """[[}]"""                                        ! "Unexpected content found: }]".failNel[JSONValue]
}