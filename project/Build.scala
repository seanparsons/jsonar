import sbt._
import Keys._
import java.io.File
import ls.Plugin._
import sbtrelease.Release._

object JSONARBuild extends Build {
  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = Defaults.defaultSettings ++ releaseSettings ++ lsSettings ++ Seq(
      scalaVersion := "2.9.1",
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4",
      libraryDependencies <+= scalaVersion{v => 
        if (v.startsWith("2.10")) "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"
        else "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
      },
      libraryDependencies <+= scalaVersion{v =>
        if (v.startsWith("2.10")) "org.specs2" % "specs2_2.9.1" % "1.7.1" % "test"
        else "org.specs2" %% "specs2" % "1.7.1" % "test"
      },
      crossScalaVersions := Seq("2.9.0-1", "2.9.1", "2.10.0-M1"),
      organization := "com.github.seanparsons.jsonar",
      name := "jsonar",
      initialCommands := """
        import com.github.seanparsons.jsonar._
        import scalaz._
        import Scalaz._
        """,
      scalacOptions ++= Seq("-deprecation"),
      LsKeys.tags in LsKeys.lsync := Seq("json", "scalaz"),
      description in LsKeys.lsync := "The JSON library that just works.",
      publishMavenStyle := true,
      publishTo <<= version { (v: String) =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT")) 
          Some("snapshots" at nexus + "content/repositories/snapshots") 
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      publishArtifact in Test := false,
      pomExtra := (
        <url>http://github.com/seanparsons/jsonar</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>http://www.opensource.org/licenses/bsd-license.php</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:seanparsons/jsonar.git</url>
          <connection>scm:git:git@github.com:seanparsons/jsonar.git</connection>
        </scm>
        <developers>
          <developer>
            <id>seanparsons</id>
            <name>Sean Parsons</name>
          </developer>
        </developers>)
    )
  )
}
