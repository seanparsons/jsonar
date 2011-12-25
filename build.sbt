scalaVersion := "2.9.1"

libraryDependencies += "org.parboiled" % "parboiled-scala" % "1.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4-M5" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFD")

organization := "com.github.seanparsons"

name := "jsonar"
