inThisBuild(Seq(
  name := "filters4s",
  organization := "de.lhns",

  scalaVersion := "2.13.6"
))

name := (ThisBuild / name).value

val V = new {
  val cats = "2.9.0"
  val circe = "0.14.5"
  val fastparse = "3.0.0"
  val munit = "0.7.29"
}

lazy val filters4s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % V.munit % Test,
      "org.typelevel" %% "cats-core" % V.cats,
      "com.lihaoyi" %% "fastparse" % V.fastparse,
      "io.circe" %% "circe-core" % V.circe,
      "io.circe" %% "circe-generic" % V.circe,
      "io.circe" %% "circe-parser" % V.circe,
    ),

    testFrameworks += new TestFramework("munit.Framework"),
  )
