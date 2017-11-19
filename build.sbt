lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization := "chords",
  version := "1.0",
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  parallelExecution in Test := false,

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
    "org.clapper" %% "grizzled-slf4j" % "1.3.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    "org.typelevel" %% "cats-core" % "1.0.0-MF"
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
