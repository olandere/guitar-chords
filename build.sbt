//enablePlugins(ScalaJSPlugin)

lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization := "chords",
  version := "1.0",
  scalaVersion := "2.13.8",
  Compile / scalacOptions ++= Seq("-deprecation", "-feature"),
  Test / parallelExecution := false,

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.4.4" % "runtime",
    "org.clapper" %% "grizzled-slf4j" % "1.3.4",
    "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    "org.scalacheck" %% "scalacheck" % "1.17.0" % "test",
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    "org.typelevel" %% "cats-core" % "2.8.0",
    "org.scala-lang" % "scala-reflect" % "2.13.10"
  ),

  publishMavenStyle := true,
  publishTo := Some(Resolver.file("Local", Path.userHome / ".m2" / "repository")),

  Compile / console / initialCommands := """
    |import cats._, cats.implicits._
    |import chord._
    |import chord.Chord._
    |import Operations._
    |""".stripMargin
)
