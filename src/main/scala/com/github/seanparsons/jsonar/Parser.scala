package com.github.seanparsons.jsonar

import annotation.tailrec
import scalaz._
import Scalaz._

sealed abstract class JSONToken {
  def originalStringContent: String
}
abstract class OpenToken extends JSONToken
abstract class CloseToken extends JSONToken
case object PrecursorToken extends JSONToken { val originalStringContent = "PRECURSOR" }
case object ArrayOpenToken extends OpenToken { val originalStringContent = "[" }
case object ArrayCloseToken extends CloseToken { val originalStringContent = "]" }
case object ObjectOpenToken extends OpenToken { val originalStringContent = "{" }
case object ObjectCloseToken extends CloseToken { val originalStringContent = "}" }
case object EntrySeparatorToken extends JSONToken { val originalStringContent = "," }
case object FieldSeparatorToken extends JSONToken { val originalStringContent = ":" }
case object StringBoundsOpenToken extends OpenToken { val originalStringContent = "\"" }
case object StringBoundsCloseToken extends CloseToken { val originalStringContent = "\"" }
case class NumberToken(originalStringContent: String) extends JSONToken
abstract class BooleanToken extends JSONToken
case object BooleanTrueToken extends BooleanToken { val originalStringContent = "true" }
case object BooleanFalseToken extends BooleanToken { val originalStringContent = "false" }
case object NullToken extends JSONToken { val originalStringContent = "null" }
abstract class StringPartToken extends JSONToken {
  def parsedStringContent: String
}
case class UnicodeCharacterToken(originalStringContent: String) extends StringPartToken {
  def parsedStringContent = new java.lang.StringBuilder().appendCodePoint(Integer.valueOf(originalStringContent.takeRight(4), 16)).toString
}
case class EscapedCharacterToken(originalStringContent: String) extends StringPartToken {
  def parsedStringContent = originalStringContent match {
    case "\\r" => "\r"
    case "\\n" => "\n"
    case "\\t" => "\t"
    case "\\b" => "\b"
    case "\\f" => "\f"
    case """\\""" => """\"""
    case """\/""" => """/"""
    case "\\\"" => "\""
    case x => x.tail
  }
}
case class NormalStringToken(originalStringContent: String) extends StringPartToken {
  def parsedStringContent = originalStringContent
}
case class UnexpectedContentToken(originalStringContent: String) extends JSONToken

object Parser {
  val UnicodeCharRegex = """(\\u[a-fA-F0-9]{4})""".r
  val EscapedCharRegex = """(\\[\\/bfnrt"])""".r
  val NormalStringRegex = """([^\"[\x00-\x1F]\\]+)""".r
  val NumberRegex = """(-?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?)""".r

  private[this] def excerpt(string: String, limit: Int = 50): String = {
    if (string.size > limit) {
      string.take(limit) + "..."
    } else {
      string
    }
  }

  private[this] def excerpt(tokens: Stream[JSONToken]): String = excerpt(tokens.map(_.originalStringContent).mkString)

  def parse(json: String): ValidationNEL[String, JSONValue] = {
    expectValue(tokenize(json)).flatMap{streamAndValue =>
      if (streamAndValue._1.isEmpty) {
        streamAndValue._2.successNel
      } else {
        "JSON contains invalid suffix content: %s".format(excerpt(streamAndValue._1)).failNel
      }
    }
  }

  def tokenize(json: String): Stream[JSONToken] = tokenize(PrecursorToken, json)

  def expectedSpacerToken(stream: Stream[JSONToken], token: JSONToken, failMessage: String): ValidationNEL[String, Stream[JSONToken]] = {
    stream match {
      case `token` #:: streamTail => streamTail.successNel
      case _ => "%s but found: %s".format(failMessage, excerpt(stream)).failNel
    }
  }
  
  def expectStringOpen(stream: Stream[JSONToken]) = expectedSpacerToken(stream, StringBoundsOpenToken, "Expected string bounds")

  def expectStringClose(stream: Stream[JSONToken]) = expectedSpacerToken(stream, StringBoundsCloseToken, "Expected string bounds")

  def expectArrayOpen(stream: Stream[JSONToken]) = expectedSpacerToken(stream, ArrayOpenToken, "Expected array open token")

  def expectArrayClose(stream: Stream[JSONToken]) = expectedSpacerToken(stream, ArrayCloseToken, "Expected array close token")

  def expectObjectOpen(stream: Stream[JSONToken]) = expectedSpacerToken(stream, ObjectOpenToken, "Expected object open token")

  def expectObjectClose(stream: Stream[JSONToken]) = expectedSpacerToken(stream, ObjectCloseToken, "Expected object close token")

  def expectEntrySeparator(stream: Stream[JSONToken]) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  def expectFieldSeparator(stream: Stream[JSONToken]) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")
  
  def expectObject(stream: Stream[JSONToken]): ValidationNEL[String, (Stream[JSONToken], JSONObject)] = {
    for {
      afterObjectOpen <- expectObjectOpen(stream)
      streamAndFields <- expectObjectField(true, (afterObjectOpen, Vector[(JSONString, JSONValue)]()).successNel)
    } yield (streamAndFields._1, JSONObject(streamAndFields._2.toMap))
  }
  
  def expectArray(stream: Stream[JSONToken]): ValidationNEL[String, (Stream[JSONToken], JSONArray)] = {
    for {
      afterArrayOpen <- expectArrayOpen(stream)
      streamAndFields <- expectArrayField(true, (afterArrayOpen, Vector[JSONValue]()).successNel)
    } yield (streamAndFields._1, JSONArray(streamAndFields._2))
  }

  def expectValue(stream: Stream[JSONToken]): ValidationNEL[String, (Stream[JSONToken], JSONValue)] = {
    stream.headOption match {
      case Some(ArrayOpenToken) => expectArray(stream)
      case Some(ObjectOpenToken) => expectObject(stream)
      case Some(StringBoundsOpenToken) => expectString(stream)
      case Some(BooleanTrueToken) => (stream.tail, JSONBoolTrue).successNel
      case Some(BooleanFalseToken) => (stream.tail, JSONBoolFalse).successNel
      case Some(NullToken) => (stream.tail, JSONNull).successNel
      case Some(NumberToken(numberText)) => {
        try {
          (stream.tail, if (numberText.toLowerCase.contains('e')) {
            JSONDecimal(BigDecimal(numberText))
          } else {
            JSONInt(BigInt(numberText))
          }).successNel
        } catch {
          case throwable => throwable.getMessage.failNel
        }
      }
      case Some(UnexpectedContentToken(excerpt)) => "Unexpected content found: %s".format(excerpt).failNel
      case Some(unexpectedToken) => "Unexpected content found: %s".format(excerpt(stream)).failNel
      case None => "JSON terminates unexpectedly".failNel
    }
  }

  @tailrec def expectArrayField(first: Boolean, currentStream: ValidationNEL[String, (Stream[JSONToken], Seq[JSONValue])]): ValidationNEL[String, (Stream[JSONToken], Seq[JSONValue])] = {
    currentStream match {
      case Success((stream, fields)) => {
        stream.headOption match {
          case Some(ArrayCloseToken) => (stream.tail, fields).successNel
          case _ => {
            expectArrayField(false, for {
              afterEntrySeparator <- if (first) stream.successNel else expectEntrySeparator(stream)
              streamAndValue <- expectValue(afterEntrySeparator)
            } yield (streamAndValue._1, fields :+ streamAndValue._2))
          }
        }
      }
      case _ => currentStream
    }
  }
  
  @tailrec def expectObjectField(first: Boolean, currentStream: ValidationNEL[String, (Stream[JSONToken], Seq[(JSONString, JSONValue)])]): ValidationNEL[String, (Stream[JSONToken], Seq[(JSONString, JSONValue)])] = {
    currentStream match {
      case Success((stream, fields)) => {
        stream.headOption match {
          case Some(ObjectCloseToken) => (stream.tail, fields).successNel
          case _ => {
            expectObjectField(false, for {
              afterEntrySeparator <- if (first) stream.successNel else expectEntrySeparator(stream)
              streamAndKey <- expectString(afterEntrySeparator)
              afterFieldSeperator <- expectFieldSeparator(streamAndKey._1)
              streamAndValue <- expectValue(afterFieldSeperator)
            } yield (streamAndValue._1, fields :+ (streamAndKey._2, streamAndValue._2)))
          }
        }
      }
      case _ => currentStream
    }
  }

  def expectString(stream: Stream[JSONToken]): ValidationNEL[String, (Stream[JSONToken], JSONString)] = {
    for {
      afterOpen <- expectStringOpen(stream)
      elements <- afterOpen.span(jsonToken => jsonToken.isInstanceOf[StringPartToken]).successNel[String]
      afterClose <- expectStringClose(elements._2)
    } yield (afterClose, JSONString(elements._1.collect{case stringPart: StringPartToken => stringPart.parsedStringContent}.mkString))
  }

  @inline def streamCons(token: JSONToken, jsonRemainder: String): Stream[JSONToken] = {
    Stream.cons(token, tokenize(token, jsonRemainder))
  }
  @inline def unexpectedContent(json: String) = Stream.cons(UnexpectedContentToken(json.take(10)), Stream.empty)
  @inline def parseStringSegments(json: String): Stream[JSONToken] = {
    if (json.head == '"') {
      streamCons(StringBoundsCloseToken, json.tail)
    } else {
      NormalStringRegex.findPrefixOf(json).map(normalStringContent => streamCons(NormalStringToken(normalStringContent), json.drop(normalStringContent.length)))
      .orElse(EscapedCharRegex.findPrefixOf(json).map(escapedChar => streamCons(EscapedCharacterToken(escapedChar), json.drop(escapedChar.length))))
      .orElse(UnicodeCharRegex.findPrefixOf(json).map(unicodeContent => streamCons(UnicodeCharacterToken(unicodeContent), json.drop(unicodeContent.length))))
      .getOrElse(unexpectedContent(json))
    }
  }

  @tailrec private[this] def tokenize(previousToken: JSONToken, json: String): Stream[JSONToken] = {
    if (json.isEmpty) Stream.empty
    else {
      previousToken match {
        case StringBoundsOpenToken => parseStringSegments(json)
        case stringPartToken: StringPartToken => parseStringSegments(json)
        case _ => {
          val jsonHead = json.head
          jsonHead match {
            case '[' => streamCons(ArrayOpenToken, json.tail)
            case ']' => streamCons(ArrayCloseToken, json.tail)
            case '{' => streamCons(ObjectOpenToken, json.tail)
            case '}' => streamCons(ObjectCloseToken, json.tail)
            case ':' => streamCons(FieldSeparatorToken, json.tail)
            case ',' => streamCons(EntrySeparatorToken, json.tail)
            case '"' => streamCons(StringBoundsOpenToken, json.tail)
            case ' ' => tokenize(previousToken, json.tail)
            case '\r' => tokenize(previousToken, json.tail)
            case '\n' => tokenize(previousToken, json.tail)
            case _ => {
              json match {
                case trueStartingJSON if trueStartingJSON.startsWith("true") => streamCons(BooleanTrueToken, json.drop(4))
                case falseStartingJSON if falseStartingJSON.startsWith("false") => streamCons(BooleanFalseToken, json.drop(5))
                case nullStartingJSON if nullStartingJSON.startsWith("null") => streamCons(NullToken, json.drop(4))
                case _ => {
                  NumberRegex.findPrefixOf(json)
                    .map(numberString => streamCons(NumberToken(numberString), json.drop(numberString.length)))
                    .getOrElse(unexpectedContent(json))
                }
              }
            }
          }
        }
      }
    }
  }
}