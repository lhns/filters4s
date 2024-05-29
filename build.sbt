inThisBuild(Seq(
  name := "filters4s",
  organization := "net.dafttech",

  scalaVersion := "2.13.12"
))

name := (ThisBuild / name).value

lazy val filters4s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
    ),

    testFrameworks += new TestFramework("munit.Framework"),
  )
