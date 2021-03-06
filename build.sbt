//enablePlugins(ScalaJSPlugin)

lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization := "chords",
  version := "1.0",
  scalaVersion := "2.13.0",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  parallelExecution in Test := false,

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
    "org.clapper" %% "grizzled-slf4j" % "1.3.4",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.typelevel" %% "cats-core" % "2.0.0-M4"
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
