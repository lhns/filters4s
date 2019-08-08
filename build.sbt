inThisBuild(Seq(
  name := "Fancy Filter for Scala",
  organization := "net.dafttech",

  scalaVersion := "2.12.8"
))

name := (ThisBuild / name).value

lazy val f2s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "io.monix" %% "monix" % "3.0.0-RC3",
      "com.lihaoyi" %% "fastparse" % "2.1.3",
      "io.circe" %% "circe-core" % "0.11.1",
      "io.circe" %% "circe-generic" % "0.11.1",
      "io.circe" %% "circe-parser" % "0.11.1"
    )
  )
