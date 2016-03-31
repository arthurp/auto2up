name := "auto2up"

organization := "org.singingwizard"

version := "0.1"

scalaVersion := "2.11.7"

//Define dependencies.
libraryDependencies ++= Seq(
    "org.apache.pdfbox"   % "pdfbox"    % "2.0.0",
    "org.rogach" %% "scallop" % "1.0.0",
    "com.jsuereth" %% "scala-arm" % "1.4"
)

mainClass in Compile := Some("org.singingwizard.auto2up.Main")

// Generate Eclipse project with sources for dependencies
EclipseKeys.withSource := true

import sbtassembly.AssemblyPlugin.defaultShellScript

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))

assemblyJarName in assembly := s"${name.value}-${version.value}"