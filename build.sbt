lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization <<= name,
  version := "1.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-optimise", "-feature", "-Yinline-warnings"),

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime",
    "org.clapper" %% "grizzled-slf4j" % "1.0.2",
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.typelevel" %% "cats" % "0.6.1"
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
