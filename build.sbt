ThisBuild / organization := "com.adamnfish"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding",
  "utf8"
)

lazy val root = (project in file(".")).settings(
  name := "aoc-2025",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.13.0",
    "org.typelevel" %% "cats-effect" % "3.6.3",
    "org.typelevel" %% "cats-effect-kernel" % "3.6.3",
    "org.typelevel" %% "cats-effect-std" % "3.6.3",
    "co.fs2" %% "fs2-core" % "3.12.2",
    "co.fs2" %% "fs2-io" % "3.12.2",
    "com.lihaoyi" %% "fastparse" % "3.1.1",
    "com.softwaremill.sttp.client3" %% "cats" % "3.11.0",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.7.0" % Test,
    "org.typelevel" %% "scalacheck-effect" % "1.0.4" % Test
  ),
  Compile / run / fork := true
)
