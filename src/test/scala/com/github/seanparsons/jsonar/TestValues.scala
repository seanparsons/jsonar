package com.github.seanparsons.jsonar

import org.scalacheck.Choose
import org.scalacheck.Gen._

object TestValues {
  val lowBigInt = BigInt(Long.MinValue) - 1000000L
  val highBigInt = BigInt(Long.MaxValue) + 1000000L
}