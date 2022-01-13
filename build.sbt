inThisBuild(Seq(
  name := "filters4s",
  organization := "net.dafttech",

  scalaVersion := "2.13.8"
))

name := (ThisBuild / name).value

lazy val filters4s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.6.1",
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
    ),

    testFrameworks += new TestFramework("munit.Framework"),
  )
