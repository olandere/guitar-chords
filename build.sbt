lazy val root = (project in file(".")).
settings(
  name := "chords",
  organization <<= name,
  version := "1.0",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-deprecation", "-optimise", "-feature"),

  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  ),

  publishMavenStyle := true,
  publishTo := Some(Resolver.file("Local", Path.userHome / ".m2" / "repository" asFile)),

  initialCommands in console := """
    |import scalaz._, syntax.show._, syntax.order._
    |import chord._
    |import chord.Chord._
    |import Operations._
    |""".stripMargin
)
