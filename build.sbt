lazy val Benchmark = config("bench") extend Test

lazy val commonSettings = Seq(
  organization := "org.gnieh",
  name := "tekstlib",
  version := "0.1.1",
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.12.2", "2.11.8"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.4",
  scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", "rootdoc.txt"),
  scalacOptions ++= Seq("-deprecation", "-feature"),
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://github.com/gnieh/tekstlib")))

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(osgiSettings)
  .settings(scalariformSettings)
  .settings(
    resourceDirectories in Compile := List(),
    OsgiKeys.exportPackage := Seq(
      "gnieh",
      "gnieh.*"),
    OsgiKeys.additionalHeaders := Map (
      "Bundle-Name" -> "Gnieh Text and Document Manipulation"),
    OsgiKeys.bundleSymbolicName := "org.gnieh.tekstlib",
    OsgiKeys.privatePackage := Seq(),
    ScalariformKeys.preferences := {
      import scalariform.formatter.preferences._
      ScalariformKeys.preferences.value
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(PreserveDanglingCloseParenthesis, true)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    // The Nexus repo we're publishing to.
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    pomIncludeRepository := { x => false },
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
    ))
	.settings(
		parallelExecution in Benchmark := false,
    logBuffered := false,
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2" % "bench",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))
	.configs(
    Benchmark)
	.settings(
		inConfig(Benchmark)(Defaults.testSettings): _*)
