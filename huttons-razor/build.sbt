name := "october-katas"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.29"
libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.21.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
