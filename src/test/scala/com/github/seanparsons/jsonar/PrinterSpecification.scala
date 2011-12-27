package com.github.seanparsons.jsonar

import org.scalatest.FeatureSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.prop.Checkers


case class PrinterSpecification() extends FeatureSpec
                                  with MustMatchers
                                  with Checkers {
  feature("print") {
    ValidResults.resultPairings.foreach{resultPairing =>
      scenario("For the JSON %s, the printed text is correct".format(resultPairing.jValue)) {
        Printer.print(resultPairing.jValue) must equal(resultPairing.json)
      }
    }
  }
}