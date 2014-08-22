organization := "org.gnieh"

name := "tekstlib"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", "rootdoc.txt")

scalacOptions ++= Seq("-deprecation", "-feature")

licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/gnieh/tekstlib"))

osgiSettings

resourceDirectories in Compile := List()

OsgiKeys.exportPackage := Seq(
  "gnieh.regex",
  "gnieh.hyphen",
  "gnieh.diff",
  "gnieh.pp"
)

OsgiKeys.additionalHeaders := Map (
  "Bundle-Name" -> "Gnieh Text and Document Manipulation"
)

OsgiKeys.bundleSymbolicName := "org.gnieh.tekstlib"

OsgiKeys.privatePackage := Seq()

defaultScalariformSettings

ScalariformKeys.preferences := {
  import scalariform.formatter.preferences._
  ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, true)
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
}

lazy val root = project in file(".")

lazy val benchmarks = project in file("benchmarks") dependsOn(root)

libraryDependencies in benchmarks += "com.github.axel22" %% "scalameter" % "0.5-M2"

testFrameworks in benchmarks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
