package com.github.seanparsons.jsonar

import annotation.tailrec

object JSONEscaping {
  def quote(string: String): String = {
    string.map{char =>
      char match {
        case '"'  => """\""""
        case '\\' => """\\"""
        case '\b' => """\b"""
        case '\f' => """\f"""
        case '\n' => """\n"""
        case '\r' => """\r"""
        case '\t' => """\t"""
        case char if (char.isControl) => "\\u%04x".format(char: Int)
        case char => char
      }
    }.mkString
  }

  def unquote(string: String): String = internalUnquote(string, new java.lang.StringBuilder())

  @tailrec
  private[this] def internalUnquote(stringStream: Seq[Char], stringBuilder: java.lang.StringBuilder): String = {
    if (stringStream.isEmpty) {
      stringBuilder.toString
    } else {
      val first = stringStream.head
      val remainder = stringStream.tail
      if (first == '\\') {
        remainder.head match {
          case 'u' => {
            val (chars, remainderRemainder) = remainder.tail.splitAt(4)
            val codePoint = Integer.parseInt(new String(chars.toArray), 16)
            stringBuilder.appendCodePoint(codePoint)
            internalUnquote(remainderRemainder, stringBuilder)
          }
          case escapedChar => {
            escapedChar match {
              case '"'  => stringBuilder.append('"')
              case '\\' => stringBuilder.append('\\')
              case '/'  => stringBuilder.append('/')
              case 'b'  => stringBuilder.append('\b')
              case 'f'  => stringBuilder.append('\f')
              case 'n'  => stringBuilder.append('\n')
              case 'r'  => stringBuilder.append('\r')
              case 't'  => stringBuilder.append('\t')
            }
            internalUnquote(remainder.tail, stringBuilder)
          }
        }
      } else {
        stringBuilder.append(first)
        internalUnquote(remainder, stringBuilder)
      }
    }
  }
}