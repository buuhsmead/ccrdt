import _root_.de.heikoseeberger.sbtheader.AutomateHeaderPlugin
import _root_.de.heikoseeberger.sbtheader.HeaderKey._
import _root_.de.heikoseeberger.sbtheader.license.Apache2_0
import _root_.scoverage.ScoverageSbtPlugin.ScoverageKeys._
import _root_.tut.Plugin._
import de.heikoseeberger.sbtheader.license.Apache2_0
import scoverage.ScoverageSbtPlugin.ScoverageKeys._

name := "Computational CRDTs"

version := "1.0"

scalaVersion := "2.11.8"

organization := "Merlijn Boogerd"

val akkaVersion = "2.4.1"
val catsVersion = "0.4.1"
val spireVersion = "0.3.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0-M9" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5",
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "cats-macros" % catsVersion,
  "org.spire-math" %% "spire" % "0.11.0",
  "com.chuusai" %% "shapeless" % "2.3.0",

  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-distributed-data-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-stream-experimental" % "2.0.2",

  "com.twitter" %% "chill" % "0.8.0"
)

resolvers += Resolver.sonatypeRepo("releases")


/* Apache-2.0 license automation */
licenses +=("Apache-2.0", url("http://opensource.org/licenses/apache2.0.php"))

headers := Map(
  "scala" -> Apache2_0("2015", "Merlijn Boogerd"),
  "conf" -> Apache2_0("2015", "Merlijn Boogerd", "#")
)

enablePlugins(AutomateHeaderPlugin)


/* Build configuration */

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
//  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

/* Coverage settings */
coverageMinimum := 60
coverageFailOnMinimum := false
coverageHighlighting := scalaBinaryVersion.value != "2.10"


/* Compiled code sections in Markdown files */
tutSettings