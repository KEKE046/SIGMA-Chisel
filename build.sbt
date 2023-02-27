val chiselVersion = "3.5.2"
val firrtlVersion = "1.5.1"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )
lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "org.apache.commons" % "commons-lang3" % "3.12.0",
  "org.apache.commons" % "commons-text" % "1.9"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full))
lazy val firrtlSettings = Seq(libraryDependencies ++= Seq("edu.berkeley.cs" %% "firrtl" % firrtlVersion))
lazy val chiselTestSettings = Seq(libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"))

lazy val SIGMA = (project in file("."))
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(chiselTestSettings)