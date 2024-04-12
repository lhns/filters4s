lazy val scalaVersions = Seq(/*"3.4.1", */ "2.13.13")

ThisBuild / scalaVersion := scalaVersions.head
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization := "de.lhns"
name := (core.projectRefs.head / name).value

val V = new {
  val betterMonadicFor = "0.3.1"
  val cats = "2.10.0"
  val circe = "0.14.6"
  val doobie = "1.0.0-RC4"
  val fastparse = "3.0.2"
  val logbackClassic = "1.5.2"
  val munit = "0.7.29"
}

lazy val commonSettings: SettingsDefinition = Def.settings(
  version := {
    val Tag = "refs/tags/v?([0-9]+(?:\\.[0-9]+)+(?:[+-].*)?)".r
    sys.env.get("CI_VERSION").collect { case Tag(tag) => tag }
      .getOrElse("0.0.1-SNAPSHOT")
  },

  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")),

  homepage := scmInfo.value.map(_.browseUrl),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/lhns/filters4s"),
      "scm:git@github.com:lhns/filters4s.git"
    )
  ),
  developers := List(
    Developer(id = "lhns", name = "Pierre Kisters", email = "pierrekisters@gmail.com", url = url("https://github.com/lhns/"))
  ),

  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % V.logbackClassic % Test,
    "org.scalameta" %%% "munit" % V.munit % Test,
  ),

  testFrameworks += new TestFramework("munit.Framework"),

  libraryDependencies ++= virtualAxes.?.value.getOrElse(Seq.empty).collectFirst {
    case VirtualAxis.ScalaVersionAxis(version, _) if version.startsWith("2.") =>
      compilerPlugin("com.olegpy" %% "better-monadic-for" % V.betterMonadicFor)
  },

  Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),

  Compile / doc / sources := Seq.empty,

  publishMavenStyle := true,

  publishTo := sonatypePublishToBundle.value,

  sonatypeCredentialHost := "s01.oss.sonatype.org",

  credentials ++= (for {
    username <- sys.env.get("SONATYPE_USERNAME")
    password <- sys.env.get("SONATYPE_PASSWORD")
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    sonatypeCredentialHost.value,
    username,
    password
  )).toList,
)

lazy val root: Project =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(
      publishArtifact := false,
      publish / skip := true
    )
    .aggregate(core.projectRefs: _*)
    .aggregate(circe.projectRefs: _*)
    .aggregate(parser.projectRefs: _*)
    .aggregate(doobie.projectRefs: _*)

lazy val core = projectMatrix.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "filters4s-core",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % V.cats,
    ),
  )
  .jvmPlatform(scalaVersions)
  .jsPlatform(scalaVersions)

lazy val circe = projectMatrix.in(file("circe"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "filters4s-circe",

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % V.circe,
      "io.circe" %% "circe-generic" % V.circe,
      "io.circe" %% "circe-parser" % V.circe,
    )
  )
  .jvmPlatform(scalaVersions)

lazy val parser = projectMatrix.in(file("parser"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "filters4s-parser",

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % V.fastparse,
    )
  )
  .jvmPlatform(scalaVersions)

lazy val doobie = projectMatrix.in(file("doobie"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name := "filters4s-doobie",

    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core" % V.doobie,
    )
  )
  .jvmPlatform(scalaVersions)
