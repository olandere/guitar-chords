lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization := "chords",
  version := "1.0",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq("-deprecation", "-feature"),

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime",
    "org.clapper" %% "grizzled-slf4j" % "1.3.0",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.typelevel" %% "cats" % "0.8.1"
  ),

  publishMavenStyle := true,
  publishTo := Some(Resolver.file("Local", Path.userHome / ".m2" / "repository" asFile)),

  initialCommands in console := """
    |import cats._, cats.implicits._
    |import chord._
    |import chord.Chord._
    |import Operations._
    |""".stripMargin
)
