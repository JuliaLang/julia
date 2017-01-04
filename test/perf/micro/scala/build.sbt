// This file is a part of Julia. License is MIT: http://julialang.org/license

libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.10",
    "org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

scalaVersion := "2.11.1"

showSuccess := false

onLoadMessage := ""

logLevel := Level.Warn
