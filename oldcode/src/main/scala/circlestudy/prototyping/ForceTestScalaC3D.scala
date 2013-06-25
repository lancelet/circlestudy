package circlestudy.prototyping

import java.io.File
import scala.collection.immutable._
import c3d._
import scalax.chart._
import scalax.chart.Charting._

object ForceTestScalaC3D extends App {

  println("===============================")
  println("Scala C3D Force Plate Data Test")
  println("===============================")
  println("")
  println("The purpose of this test is to read data from one of the C3D data files and")
  println("produce some plots.  This will allow the force plate data that has been")
  println("read by scala-c3d to be compared with output from Mokka.  This is just a")
  println("verification step to make sure that the force plate channels are read")
  println("correctly.")

  // Load a test trial: Horse3_circle_left_trot_3.c3d
  val inFile: File = new File("./data/Horse3/Horse 3 hard surface circle/Horse3_circle_left_trot_3.c3d")
  val c3d: C3D = C3D.read(inFile).getOrElse{ println("Could not load file."); sys.exit }

  // Get ForcePlate 6
  val fp6: ForcePlate = c3d.forcePlates(6 - 1)

  // Extract fx, fy and fz sequences
  val fSeq: IndexedSeq[Vec3D] = fp6.force
  val mag: IndexedSeq[Float] = fSeq map (_.mag)

  // Plot
  val dataset = mag.zipWithIndex.map(x => (x._2 / 1000.0, x._1)).toXYSeriesCollection()
  val chart = XYLineChart(dataset, legend=false)
  chart.rangeAxisLabel = "|F| (N)"
  chart.domainAxisLabel = "time (s)"
  chart.title = "Horse3_circle_left_trot_3 Force Plate 6 Magnitude"
  chart.show
  chart.saveAsPNG(new File("test-grf.png"), (600,300))

}
