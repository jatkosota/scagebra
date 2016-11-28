name := "scagebra"

version := "1.0"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion
)

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-language:implicitConversions"
)

initialCommands in console := "import scalaz._, Scalaz._"
