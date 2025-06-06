scalaVersion := "3.3.5"


val http4sVersion = "0.23.30"

libraryDependencies ++= Seq(
    "org.http4s" %% "http4s-ember-client" % http4sVersion,
    "org.http4s" %% "http4s-ember-server" % http4sVersion,
    "org.http4s" %% "http4s-dsl"          % http4sVersion,
    "org.typelevel" %% "cats-core" % "2.10.0",
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "io.circe" %% "circe-generic" % "0.14.8",
    "io.circe" %% "circe-parser" % "0.14.8"
)


// libraryDependencies +=
//     "org.typelevel" %% "cats-core" % "2.10.0"

scalacOptions ++= Seq(
    "-Xfatal-warnings"
)