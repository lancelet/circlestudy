import sbt._
import sbt.Keys._

object BuildSettings {
  val buildSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
    organization := "com.github.circlestudy",
    scalaVersion := "2.10.1",
    version      := "0.1"
  )
}

object Resolvers {
  val allResolvers: Seq[MavenRepository] = Seq(
    "Artenum"  at "http://maven.artenum.com/content/repositories/thirdparty/",
    "Sonatype" at "https://oss.sonatype.org/content/groups/public"
  )
}

object Dependencies {
  val allDependencies: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  )
}

object CircleStudyBuild extends Build {
  import BuildSettings._
  import Resolvers._
  import Dependencies._

  lazy val circlestudy = Project(
    "circlestudy",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies := allDependencies,
      resolvers           := allResolvers
    )
  ) dependsOn (
    mocaputils
  )

  val mocaputilsUri = uri("git://github.com/lancelet/mocaputils.git")
  lazy val mocaputils = RootProject(mocaputilsUri)
}
