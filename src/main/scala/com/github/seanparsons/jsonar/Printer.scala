package com.github.seanparsons.jsonar

import scalaz._
import Scalaz._

object Printer {
  def compact(json: JSONValue): String = prettyPrint(json, "", "", ":", "")

  def pretty(json: JSONValue): String = prettyPrint(json, "    ", "\n", ": ", "")

  def prettyPrint(json: JSONValue, tab:String, delimbreak: String, colon: String, currentindent: String): String = {

    def join(s: Seq[String]) = s.mkString("," + delimbreak + currentindent + tab)

    def recurse(json: JSONValue) = prettyPrint(json, tab, delimbreak, colon, currentindent + tab)

    def bracket(s: String, open: String, close: String) =
      open + delimbreak + currentindent + tab + s + delimbreak + currentindent + close

    def entries[A](entries: Seq[A], open: String, close: String, f: A => String) =
      bracket(join(entries map f), open, close)

    def printString(s: String) = '"' + JSONEscaping.quote(s) + '"'

    def isComplex(json: JSONValue): Boolean = json.fold(false, _ => false, _ => false, _ => false, _ => true, _ => true)

    json.fold (
      "null",
      bool => bool.toString,
      number => number.toString,
      string => printString(string),
      elements => entries(elements, "[", "]", recurse),
      fields => entries(fields.toSeq, "{", "}", {(pair: Tuple2[JSONString, JSONValue]) =>
        printString(pair._1.value) + colon + (if (isComplex(pair._2)) delimbreak + currentindent + tab else "") + recurse(pair._2)
      })
    )
  }
}