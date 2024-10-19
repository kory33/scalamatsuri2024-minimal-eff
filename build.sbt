scalaVersion := "3.6.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores"
)

val http4sVersion = "1.0.0-M41"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.typelevel" %% "log4cats-slf4j" % "2.7.0"
)
