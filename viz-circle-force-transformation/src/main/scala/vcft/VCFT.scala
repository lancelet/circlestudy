package vcft

import java.io.File
import c3d.C3D
import java.awt.geom.{Point2D, AffineTransform}

object VCFT {

  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")
  val outDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/vcft")
  val markerOnlyDir: File = new File(outDir, "markers-only")
  val trialFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_circle_left_trot_3.c3d")
  val staticFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_static_virtual_2.c3d")
  lazy val trial: C3D = C3D.read(trialFile)
  lazy val static: C3D = C3D.read(staticFile)

  def main(args: Array[String]) {

    println("VCFT: Visualizing Circle Force Transformation")

    println("Creating required output directories.")
    createOutputDirsIfNecessary()


  }

  /**
   * Creates output directories if they don't already exist.
   */
  private def createOutputDirsIfNecessary() {
    def mkdir(dir: File) = if (!dir.exists()) dir.mkdirs()
    mkdir(outDir)
    mkdir(markerOnlyDir)
  }

  /**
   * 2D transformations.
   */
  trait XForm2D {
    def transform(p: Point2D): Point2D
    def scale(x: Float, y: Float): XForm2D
    def translate(x: Float, y: Float): XForm2D
  }
  object XForm2D {
    def apply(): XForm2D = new DefaultXForm2D(new AffineTransform())
  }
  class DefaultXForm2D(private val affine: AffineTransform) extends XForm2D {
    def transform(p: Point2D): Point2D = {
      val pDest = new Point2D.Float()
      affine.transform(p, pDest)
      pDest
    }
    def scale(x: Float, y: Float): XForm2D = {
      val affineNew = new AffineTransform(affine)
      affineNew.scale(x, y)
      new DefaultXForm2D(affineNew)
    }
    def translate(x: Float, y: Float): XForm2D = {
      val affineNew = new AffineTransform(affine)
      affineNew.translate(x, y)
      new DefaultXForm2D(affineNew)
    }
  }

}
