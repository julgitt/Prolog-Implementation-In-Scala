ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Prolog",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.0",
      "org.jline" % "jline" % "3.25.1"
    )
  )
