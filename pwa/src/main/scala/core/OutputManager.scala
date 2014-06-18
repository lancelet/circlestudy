package core

import java.io.File

import org.joda.time.DateTime

import scalaz._, Scalaz._


class OutputManager(outputDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output")) {

  /**
   * Creates a date and time stamped output directory.
   *
   * The new directory is created as a sub-directory of `outputDir`, with the format:
   *   YYYYMMDDTHHMMSS
   * where HH is expressed in 24-hour format
   *
   * The purpose of this is to prevent over-writing previous results on a new run.
   *
   * @return date and time stamped output directory
   */
  def createTimeStampOutputDir: String \/ File = {
    val now: DateTime = DateTime.now()

    val yr = f"${now.getYear}%4d"
    val mo = f"${now.getMonthOfYear}%02d"
    val dy = f"${now.getDayOfMonth}%02d"
    val hr = f"${now.getHourOfDay}%02d"
    val mn = f"${now.getMinuteOfHour}%02d"
    val sc = f"${now.getSecondOfMinute}%02d"
    val name = s"${yr}${mo}${dy}T${hr}${mn}${sc}"

    val dir = new File(outputDir, name)

    if (dir.exists) {
      s"Directory ${dir.getCanonicalPath.toString} already exists".left
    } else if (dir.mkdirs() == false) {
      s"Could not create directory ${dir.getCanonicalPath.toString}".left
    } else {
      dir.right
    }
  }

}
