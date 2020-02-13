inThisBuild(Seq(
  name := "Fancy Filter for Scala",
  organization := "net.dafttech",

  scalaVersion := "2.13.1"
))

name := (ThisBuild / name).value

lazy val f2s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "io.monix" %% "monix" % "3.1.0",
      "org.typelevel" %% "cats-core" % "2.1.0",
      "com.lihaoyi" %% "fastparse" % "2.2.2",
      "io.circe" %% "circe-core" % "0.13.0",
      "io.circe" %% "circe-generic" % "0.13.0",
      "io.circe" %% "circe-parser" % "0.13.0"
    )
  )
