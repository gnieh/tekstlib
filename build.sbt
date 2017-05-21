lazy val commonSettings = Seq(
  organization := "org.gnieh",
  name := "tekstlib",
  version := "0.2.0-SNAPSHOT",
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
    publishTo := {
      val v = version.value
      val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
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

lazy val benchmarks = project.in(file("benchmarks"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.8",
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    logBuffered := false,
    scalacOptions ++= Seq("-deprecation", "-feature"))
  .aggregate (root)
