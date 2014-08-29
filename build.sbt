name := "music"

organization <<= name

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-optimise", "-feature")

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.0"
)

publishMavenStyle := true

publishTo := Some(Resolver.file("Local", Path.userHome / ".m2" / "repository" asFile))
