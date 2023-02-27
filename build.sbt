val chiselVersion = "3.5.4"
val firrtlVersion = "1.5.1"
val chiselTestVersion = "0.5.4"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11")
  )
lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full))
lazy val firrtlSettings = Seq(libraryDependencies ++= Seq("edu.berkeley.cs" %% "firrtl" % firrtlVersion))
lazy val chiselTestSettings = Seq(libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % chiselTestVersion % "test"))

lazy val SIGMA = (project in file("."))
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(chiselTestSettings)
  .settings(libraryDependencies ++= Seq("edu.berkeley.cs" %% "rocketchip" % "1.2.+"))