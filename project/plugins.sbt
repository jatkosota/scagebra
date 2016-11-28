// or clone this repo and type `sbt publishLocal`
resolvers += Resolver.sonatypeRepo("snapshots")

// update to the latest development version, see project/EnsimeSbtBuild.scala
addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.0")
