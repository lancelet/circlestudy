package vcft

import java.io.File
import c3d.{Vec3D, C3D}
import java.awt.geom.{Path2D, Point2D, AffineTransform}
import java.awt.{BasicStroke, Color, RenderingHints, Graphics2D}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.collection.immutable._

object VCFT {

  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")
  val outDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/vcft")
  val markerOnlyDir: File = new File(outDir, "markers-only")
  val trialFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_circle_left_trot_3.c3d")
  val staticFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_static_virtual_2.c3d")
  lazy val trial: C3D = C3D.read(trialFile)
  lazy val static: C3D = C3D.read(staticFile)
  val xRes: Int = 800
  val yRes: Int = 600
  val startFrame: Int = 175
  val endFrame: Int = 230
  val viewRadius: Float = 4000.0f
  val colorA: Color = new Color(86, 117, 168)
  val colorB: Color = new Color(81, 163, 206)
  val fpFill: Color = colorA
  val fpDraw: Color = colorB

  def main(args: Array[String]) {

    println("VCFT: Visualizing Circle Force Transformation")

    println("Creating required output directories.")
    createOutputDirsIfNecessary()

    println("Rendering marker-only pass.")
    renderMarkersOnly()

  }

  /**
   * Renders marker-only.
   */
  private def renderMarkersOnly() {
    def renderFrame(frame: Int) {
      println(s"Rendering marker only frame: $frame")
      val outFile: File = new File(markerOnlyDir, f"$frame%05d.png")
      val ImageGraphicPair(image, g) = createImage()
      val renderInfo = RenderInfo(frame, g, xForm, trial, static)
      renderForcePlate(0, renderInfo)
      g.dispose()
      ImageIO.write(image, "PNG", outFile)
    }
    for (frame <- (startFrame until endFrame).par) renderFrame(frame)
  }

  case class ImageGraphicPair(image: BufferedImage, g2d: Graphics2D)
  def createImage(): ImageGraphicPair = {
    import RenderingHints._
    val image = new BufferedImage(xRes, yRes, BufferedImage.TYPE_INT_RGB)
    val g2d = image.createGraphics()
    g2d.setRenderingHint(KEY_ANTIALIASING,    VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(KEY_RENDERING,       VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(KEY_COLOR_RENDERING, VALUE_COLOR_RENDER_QUALITY)
    ImageGraphicPair(image, g2d)
  }

  case class RenderInfo(frame: Int, g2d: Graphics2D, xForm: XForm2D, c3d: C3D, static: C3D)
  def renderForcePlate(number: Int, r: RenderInfo) {
    val corners: IndexedSeq[Vec3D] = r.c3d.platforms.plates(number).corners
    val path: Path2D = {
      val p = new Path2D.Float()
      p.moveTo(corners(0).x, corners(0).y)
      p.lineTo(corners(1).x, corners(1).y)
      p.lineTo(corners(2).x, corners(2).y)
      p.lineTo(corners(3).x, corners(3).y)
      p.closePath()
      p.transform(r.xForm.asAffine)
      p
    }
    val g = r.g2d
    g.setColor(fpFill)
    g.fill(path)
    g.setColor(fpDraw)
    g.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    g.draw(path)
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
  lazy val xForm: XForm2D = { // default transformation
    val diam = 2.0f * viewRadius
    val uScale = math.min(xRes, yRes) / diam
    XForm2D().scale(1, -1).scale(uScale).translate(viewRadius, -viewRadius)
  }
  trait XForm2D {
    def transform(p: Point2D): Point2D
    def scale(x: Float, y: Float): XForm2D
    def scale(u: Float): XForm2D = scale(u, u)
    def translate(x: Float, y: Float): XForm2D
    def inverse: XForm2D
    def asAffine: AffineTransform
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
    def inverse: XForm2D = {
      val affineNew = new AffineTransform(affine)
      affineNew.invert()
      new DefaultXForm2D(affineNew)
    }
    val asAffine: AffineTransform = affine
  }

  implicit class RichColor(c: Color) {
    def r: Int = c.getRed
    def g: Int = c.getGreen
    def b: Int = c.getBlue
    def withAlpha(alpha: Float) = new Color(r, g, b, (alpha * 255).toInt)
  }

}
