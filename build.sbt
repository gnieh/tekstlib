organization := "org.gnieh"

name := "tekstlib"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.11.4", "2.10.4")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

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
  "gnieh.mustache",
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

publishMavenStyle := true

publishArtifact in Test := false

// The Nexus repo we're publishing to.
publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { x => false }

pomExtra := (
  <scm>
    <url>https://github.com/gnieh/tekstlib</url>
    <connection>scm:git:git://github.com/gnieh/tekstlib.git</connection>
    <developerConnection>scm:git:git@github.com:gnieh/tekstlib.git</developerConnection>
    <tag>HEAD</tag>
  </scm>
  <developers>
    <developer>
      <id>satabin</id>
      <name>Lucas Satabin</name>
      <email>lucas.satabin@gnieh.org</email>
    </developer>
  </developers>
  <ciManagement>
    <system>travis</system>
    <url>https://travis-ci.org/#!/gnieh/tekstlib</url>
  </ciManagement>
  <issueManagement>
    <system>github</system>
    <url>https://github.com/gnieh/tekstlib/issues</url>
  </issueManagement>
)

lazy val benchmarks = project in file("benchmarks") dependsOn(root)

libraryDependencies in benchmarks += "com.github.axel22" %% "scalameter" % "0.5-M2"

testFrameworks in benchmarks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
