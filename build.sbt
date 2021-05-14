inThisBuild(Seq(
  name := "filters4s",
  organization := "net.dafttech",

  scalaVersion := "2.13.5"
))

name := (ThisBuild / name).value

lazy val f2s = project.in(file("."))
  .settings(
    version := "0.0.0",

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.25" % Test,
      "io.monix" %% "monix" % "3.4.0",
      "org.typelevel" %% "cats-core" % "2.4.2",
      "com.lihaoyi" %% "fastparse" % "2.3.2",
      "io.circe" %% "circe-core" % "0.13.0",
      "io.circe" %% "circe-generic" % "0.13.0",
      "io.circe" %% "circe-parser" % "0.13.0"
    ),

    testFrameworks += new TestFramework("munit.Framework"),
  )
