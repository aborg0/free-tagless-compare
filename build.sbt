organization  := "com.softwaremill"

name := "free-tagless-compare"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.1"

val catsVersion = "2.1.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-kernel" % catsVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "org.typelevel" %% "cats-tagless-core" % "0.11",
  "dev.zio" %% "zio" % "1.0.0-RC18-2",
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
