name := "scagebra"

version := "1.0"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-deprecation"
)

initialCommands in console := "import scalaz._, Scalaz._" ++
  ", scagebra._, polynomial._" ++
  ", Monomial.Implicits._, Term.Implicits._, Polynomial.Implicits._, Rational.Implicits._" ++
  ", Groebner._\n" ++
  "val x = \"x\"; val y = \"y\"; val z = \"z\""
