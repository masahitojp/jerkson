/* basic project info */
name := "jerkson"

organization := "org.cphylabs"

version := "0.6.4-SNAPSHOT"

licenses := Seq(
  ("The MIT License", url("http://codahale.com/mit.txt"))
)

homepage := Some(url("https://github.com/cphylabs/jerkson"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/cphylabs/jerkson"),
    "scm:git:https://github.com/cphylabs/jerkson.git",
    Some("scm:git:git@github.com:cphylabs/jerkson.git")
  )
)

/* scala versions and options */
scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.2")

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  // "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8",
  "-optimise"
)

scalacOptions ++= Seq(
  "-Yclosure-elim",
  "-Yinline"
)

// These language flags will be used only for 2.10.x.
// Uncomment those you need, or if you hate SIP-18, all of them.
scalacOptions <++= scalaVersion map { sv =>
  if (sv startsWith "2.10") List(
    // "-Xverify",
    // "-Ywarn-all",
    "-feature",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials"
  )
  else Nil
}

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

/* dependencies */
libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-core" % "2.4.1",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.1",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

libraryDependencies <+= scalaVersion {
  "org.scala-lang" % "scala-reflect" % _
}

resolvers ++= Seq(
  "Local Maven" at Path.userHome.asFile.toURI.toURL + ".m2/repository"
)


/* testing */
parallelExecution in Test := false

/* sbt behavior */
logLevel in compile := Level.Warn

traceLevel := 5

offline := false

/* publishing */
publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some(
    "snapshots" at nexus + "content/repositories/snapshots"
  )
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>cphylabs</id>
      <name>CloudPhysics Inc</name>
      <email>opensource@cloudphysics.com</email>
    </developer>
  </developers>
)
