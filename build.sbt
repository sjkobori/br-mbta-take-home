val scala3Version = "3.2.2"
val http4sVersion = "1.0.0-M39"


lazy val root = project
  .in(file("."))
  .settings(
    name := "http4sClientTest",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-dsl"          % http4sVersion,
    ),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-circe" % http4sVersion,
      // Optional for auto-derivation of JSON codecs
      "io.circe" %% "circe-generic" % "0.14.3",
      // Optional for string interpolation to JSON model
      "io.circe" %% "circe-literal" % "0.14.3"
    ),
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
  
  )
