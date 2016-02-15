val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"

lazy val commonSettings = Seq(
  organization := "org.draegisoft",
  version := "0.0.1"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "squamata",
    libraryDependencies += scalaTest
  )
