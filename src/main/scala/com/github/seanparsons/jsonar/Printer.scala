package com.github.seanparsons.jsonar

object Printer {
  def print(jsonValue: JSONValue): String = {
    val stringBuilder = new StringBuilder()
    appendJSONValue(stringBuilder, jsonValue)
    stringBuilder.toString
  }
  @inline def appendJSONNull(stringBuilder: StringBuilder): Unit = stringBuilder.append("null")
  @inline def appendJSONString(stringBuilder: StringBuilder, jsonString: JSONString) {
    stringBuilder.append('"')
    stringBuilder.append(JSONEscaping.quote(jsonString.value))
    stringBuilder.append('"')
  }
  @inline def appendJSONNumber(stringBuilder: StringBuilder, jsonNumber: JSONNumber) = stringBuilder.append(jsonNumber.value.toString)
  @inline def appendJSONBoolTrue(stringBuilder: StringBuilder) = stringBuilder.append("true")
  @inline def appendJSONBoolFalse(stringBuilder: StringBuilder) = stringBuilder.append("false")
  def appendJSONObject(stringBuilder: StringBuilder, jsonObject: JSONObject) {
    stringBuilder.append('{')
    jsonObject.fields.headOption.foreach{pair =>
      appendJSONString(stringBuilder, pair._1)
      stringBuilder.append(':')
      appendJSONValue(stringBuilder, pair._2)
    }
    if (!jsonObject.fields.isEmpty) {
      jsonObject.fields.tail.foreach{pair =>
        stringBuilder.append(',')
        appendJSONString(stringBuilder, pair._1)
        stringBuilder.append(':')
        appendJSONValue(stringBuilder, pair._2)
      }
    }
    stringBuilder.append('}')
  }
  def appendJSONArray(stringBuilder: StringBuilder, jsonArray: JSONArray) {
    stringBuilder.append('[')
    jsonArray.elements.headOption.foreach(jsonValue => appendJSONValue(stringBuilder, jsonValue))
    if (!jsonArray.elements.isEmpty) {
      jsonArray.elements.tail.foreach{jsonValue =>
        stringBuilder.append(',')
        appendJSONValue(stringBuilder, jsonValue)
      }
    }
    stringBuilder.append(']')
  }
  def appendJSONValue(stringBuilder: StringBuilder, jsonValue: JSONValue) {
    jsonValue match {
      case jsonObject: JSONObject => appendJSONObject(stringBuilder, jsonObject)
      case jsonArray: JSONArray => appendJSONArray(stringBuilder, jsonArray)
      case jsonString: JSONString => appendJSONString(stringBuilder, jsonString)
      case jsonNumber: JSONNumber=> appendJSONNumber(stringBuilder, jsonNumber)
      case JSONBoolTrue => appendJSONBoolTrue(stringBuilder)
      case JSONBoolFalse => appendJSONBoolFalse(stringBuilder)
      case jsonNull: JSONNull => appendJSONNull(stringBuilder)
    }
  }
}