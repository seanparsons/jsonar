import sbt._
import Keys._
import java.io.File

object JSONARBuild extends Build {
  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      scalaVersion := "2.9.1",
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3",
      libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test",
      libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
      crossScalaVersions := Seq("2.9.0-1", "2.9.1"),
      testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oFD"),
      organization := "com.github.seanparsons.jsonar",
      name := "jsonar",
      version := "0.8.3",
      initialCommands := """
        import com.github.seanparsons.jsonar._
        import scalaz._
        import Scalaz._
        """,
      scalacOptions ++= Seq("-deprecation"),
      publishTo := Some(Resolver.file("file",  new File(System.getProperty("maven.repo", "./target/maven-repo"))))
    )
  )
}