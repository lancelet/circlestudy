import sbt._
import Keys._

object Common {
  val commonSettings = Seq(
  )
  def CircleStudyProject(name: String) = Project(name, file(name)).settings(commonSettings:_*)
}
