import sbt._
import Keys._

/** All dependencies for the project should be listed here. */
object Dependencies {

  // Libraries
  // val specs2 = "org.specs2" %% "specs2" % "1.14"  // for example
  val commonsMath = "org.apache.commons" % "commons-math3" % "3.2"
  val jodaTime    = "joda-time"          % "joda-time"     % "2.3"
  val jodaConvert = "org.joda"           % "joda-convert"  % "1.6"

  // Projects
  val hoofEventComparisonDependencies = Seq()
  val pwaDependencies = Seq(commonsMath, jodaTime, jodaConvert)
}
