package pwa

import java.io.File

import scala.collection.immutable._
import scala.io.Source


// TEMPORARY: Checks for any differences in stride timing exports
object CheckDiff extends App {

  val d1: File = new File("/Users/jsm/Documents/dev/circlestudy/output_backup/stride-timings")
  val d2: File = new File("/Users/jsm/Documents/dev/circlestudy/output/20140810T190005/stride-timings")

  val d1Files: Seq[File] = d1.listFiles.to[Seq].filterNot(_.getName.startsWith("."))
  val d2Files: Seq[File] = d2.listFiles.to[Seq].filterNot(_.getName.startsWith("."))

  val matchedFiles: Seq[(File, Option[File])] = {
    val missingD1Files = d2Files.map(_.getName).toSet -- d1Files.map(_.getName).toSet
    if (!missingD1Files.isEmpty) {
      println("Missing d1 files: " ++ missingD1Files.toString)
    }
    for (d1file <- d1Files) yield (d1file, d2Files.find(_.getName == d1file.getName))
  }

  final case class Stride(limb: String, start: Int, end: Int)

  def stridesFromFile(inFile: File): Seq[Stride] = {
    val tlines: Seq[String] = try {
      Source.fromFile(inFile).getLines().drop(3).to[Seq]
    } catch {
      case ex: Exception => println(s"could not read file: ${inFile}"); throw ex
    }
    for {
      line <- tlines
      if !line.trim.isEmpty
    } yield {
      val items = line.split(",")
      Stride(items(0), items(1).toInt, items(2).toInt)
    }
  }

  for {
    (d1file, d2optfile) <- matchedFiles
  } {
    val d1Strides = stridesFromFile(d1file)
    d2optfile match {
      case Some(d2file) =>
        val d2Strides = stridesFromFile(d2file)
        if (d1Strides != d2Strides) {
          println("=== File differs: " ++ d1file.getName)
          println("d1:")
          println(d1Strides)
          println("d2:")
          println(d2Strides)
        }
      case None         => println("Missing d2 file: " ++ d1file.getName)
    }
  }

}
