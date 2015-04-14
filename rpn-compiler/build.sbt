name := "rpn-compiler"

organization := "com.loopfor.rpn"

version := "0.1"

description := "RPN Compiler"

licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-encoding", "UTF-8"
)

javacOptions ++= Seq(
  "-source", "1.6",
  "-target", "1.6"
)

packSettings

packMain := Map("rpnc" -> "com.loopfor.rpn.compiler.Compiler")

publishTo := Some(
  if (version.value endsWith "SNAPSHOT")
    "Sonatype Nexus Snapshot Repository" at "https://oss.sonatype.org/content/repositories/snapshots/"
  else
    "Sonatype Nexus Release Repository" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
)

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>davidledwards</id>
      <name>David Edwards</name>
      <email>david.l.edwards@gmail.com</email>
    </developer>
  </developers>
)
