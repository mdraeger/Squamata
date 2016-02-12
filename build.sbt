lazy val commonSettings = Seq(
  organization := "org.draegisoft",
  version := "0.0.1"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "generic_math"
  )
