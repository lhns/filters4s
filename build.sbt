inThisBuild(Seq(
  name := "filters4s",
  organization := "net.dafttech",

  scalaVersion := "2.13.6"
))

name := (ThisBuild / name).value

val V = new {
  val circe = "0.14.1"
}

lazy val filters4s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.7.0",
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      "io.circe" %% "circe-core" % V.circe,
      "io.circe" %% "circe-generic" % V.circe,
      "io.circe" %% "circe-parser" % V.circe,
    ),

    testFrameworks += new TestFramework("munit.Framework"),
  )
