import sbt._
import Keys._
import ls.Plugin._
import de.johoop.jacoco4sbt._
import JacocoPlugin._

object JSONARBuild extends Build {
  lazy val project = Project(
    id = "root",
    base = file("."),
    settings = Defaults.defaultSettings ++ jacoco.settings ++ lsSettings ++ Seq(
      scalaVersion := "2.9.2",
      resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
      /*
      libraryDependencies <+= (scalaVersion){scalaVersion => scalaVersion match {
        case "2.9.1-1" => "org.scalaz" % "scalaz-core_2.9.1" % "7.0-SNAPSHOT"
        //case "2.10.0-M3" => "org.scalaz" % "scalaz-core_2.9.2" % "7.0-SNAPSHOT"
        case _ => "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"
      }},
      */
      libraryDependencies <+= (scalaVersion){scalaVersion => scalaVersion match {
        //case _ => "org.scalacheck" %% "scalacheck" % "1.10-SNAPSHOT" % "test"
        case "2.9.1-1" => "org.scalacheck" % "scalacheck_2.9.1" % "1.9" % "test"
        case _ => "org.scalacheck" %% "scalacheck" % "1.9" % "test"
      }},
      libraryDependencies <+= (scalaVersion){scalaVersion => scalaVersion match {
        case _ => "org.specs2" %% "specs2" % "1.9" % "test"
      }},
      crossScalaVersions := Seq("2.9.1", "2.9.1-1", "2.9.2"), // ++ (1 to 3).map("2.10.0-M" + _.toString),
      organization := "com.github.seanparsons.jsonar",
      name := "jsonar",
      version := "0.9.3",
      initialCommands := """
        import com.github.seanparsons.jsonar._
        import scalaz._
        import Scalaz._
        """,
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise"),
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
