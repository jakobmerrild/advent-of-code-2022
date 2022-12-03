val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-effect" % "3.4.2",
      "com.monovore" %% "decline" % "2.4.0",
      "com.monovore" %% "decline-effect" % "2.4.0",
      "co.fs2" %% "fs2-core" % "3.4.0",
      "co.fs2" %% "fs2-io" % "3.4.0"
    )
  )
