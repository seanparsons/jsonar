package org.scalatest

import org.scalatest.matchers.{MatchResult, Matcher}
import scalaz._
import Scalaz._

trait ScalaTestScalazSupport {
  def equalByType[T](right: T)(implicit equal: Equal[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T) =
        MatchResult(
          left === right,
          FailureMessages("didNotEqual", left, right),
          FailureMessages("equaled", left, right)
        )
    }
}