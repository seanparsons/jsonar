package com.github.seanparsons.jsonor

object Printer {
  def toString(jsonValue: JSONValue): String = jsonValue.textElements.mkString
}