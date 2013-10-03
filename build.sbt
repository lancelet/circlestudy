import Dependencies._

import Common._

scalaVersion := "2.10.3"

val scalaC3DUri = uri("../scala-c3d")

lazy val scalac3d = ProjectRef(scalaC3DUri, "c3d")

lazy val hoofEventComparison = (
  CircleStudyProject("hoofEventComparison").
    settings(hoofEventComparisonDependencies:_*).
    dependsOn(scalac3d)
)

lazy val pwa = {
  CircleStudyProject("pwa").
    settings(pwaDependencies:_*).
    dependsOn(scalac3d)
}
