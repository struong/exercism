scalaVersion := "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "2.7.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings"
)