package vcft

import java.io.File
import c3d.{Point, Vec3D, C3D}
import java.awt.geom._
import java.awt.{BasicStroke, Color, RenderingHints, Graphics2D}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.collection.immutable._
import pwa.Buchner

object VCFT {

  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")
  val outDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/vcft")
  val markerOnlyDir: File = new File(outDir, "markers-only")
  val comOnlyDir: File = new File(outDir, "com-only")
  val trialFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_circle_left_trot_6.c3d")
  val staticFile: File = new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_static_virtual_2.c3d")
  lazy val trial: C3D = C3D.read(trialFile)
  lazy val static: C3D = C3D.read(staticFile)
  val xRes: Int = 800
  val yRes: Int = 600
  val startFrame: Int = 140
  val endFrame: Int = 210
  val nSubFrames: Int = 2
  val viewRadius: Float = 1000.0f
  val viewXShift: Float = 0.0f
  val viewYShift: Float = -100.0f
  val markerDiam: Float = 20.0f
  val stanceStart: Int = 165
  val stanceEnd: Int = 199
  // colors: http://colorschemedesigner.com/#3p31Tw0w0iiJa
  val colorA: Color = colorFromHex("06799F")
  val colorB: Color = colorFromHex("03617F")
  val colorC: Color = colorFromHex("58C1E4")
  val colorD: Color = colorFromHex("FFCF5C")
  val colorE: Color = colorFromHex("FF755C")
  val fpActiveFill: Color = colorA
  val fpInactiveFill: Color = colorB
  val fpDraw: Color = colorC
  val markerFill: Color = colorD
  val butterFlyDraw: Color = colorC.withAlpha(0.4f)
  val forceDraw: Color = colorC
  val comColor: Color = colorE
  val fScale: Float = 1.0f
  val comDiam: Float = 160.0f

  def main(args: Array[String]) {

    println("VCFT: Visualizing Circle Force Transformation")

    println("Creating required output directories.")
    createOutputDirsIfNecessary()

    println("Rendering marker-only pass.")
    renderMarkersOnly()

    println("Rendering COM pass.")
    renderCOMOnly()

  }

  /**
   * Renders marker-only.
   */
  private def renderMarkersOnly() {
    def renderSubFrame(frame: Int, subFrame: Int) {
      println(s"Rendering marker only frame: $frame, subframe: $subFrame")
      val ImageGraphicPair(image, g) = createImage()
      val renderInfo = RenderInfo(frame, subFrame, g, xForm, trial, static)
      val outFile: File = new File(markerOnlyDir, f"${renderInfo.frameNumber}%05d.png")
      renderForcePlate(0, renderInfo)
      renderPoints(renderInfo)
      renderForceButterfly(0, renderInfo)
      renderSingleForce(0, renderInfo)
      g.dispose()
      ImageIO.write(image, "PNG", outFile)
    }
    def renderFrame(frame: Int) = for (subframe <- 0 until nSubFrames) renderSubFrame(frame, subframe)
    for (frame <- (startFrame until endFrame).par) renderFrame(frame)
  }

  /**
   * Renders COM + markers.
   */
  private def renderCOMOnly() {
    def renderSubFrame(frame: Int, subFrame: Int) {
      println(s"Rendering COM frame: $frame, subframe: $subFrame")
      val ImageGraphicPair(image, g) = createImage()
      val renderInfo = RenderInfo(frame, subFrame, g, xForm, trial, static)
      val outFile: File = new File(comOnlyDir, f"${renderInfo.frameNumber}%05d.png")
      renderForcePlate(0, renderInfo)
      renderPoints(renderInfo)
      renderForceButterfly(0, renderInfo)
      renderSingleForce(0, renderInfo)
      renderCOM(renderInfo)
      g.dispose()
      ImageIO.write(image, "PNG", outFile)
    }
    def renderFrame(frame: Int) = for (subframe <- 0 until nSubFrames) renderSubFrame(frame, subframe)
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

  case class RenderInfo(frame: Int, subFrame: Int, g2d: Graphics2D, xForm: XForm2D, c3d: C3D, static: C3D) {
    val frameNumber: Int = frame * nSubFrames + subFrame
    val frameFraction: Float = subFrame.toFloat / nSubFrames.toFloat
  }

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
    g.setColor(if (frameInStance(r.frame)) fpActiveFill else fpInactiveFill)
    g.fill(path)
    g.setColor(fpDraw)
    g.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    g.draw(path)
  }

  def renderPoints(r: RenderInfo) {
    val rad = markerDiam / 2.0f
    r.g2d.setColor(markerFill)
    for {
      point <- r.c3d.points.points
      v1 <- point(r.frame)
      v2 <- point(r.frame + 1)
      v = v1 * (1.0f - r.frameFraction) + v2 * r.frameFraction
    } {
      val e = new Ellipse2D.Float(v.x - rad, v.y - rad, markerDiam, markerDiam)
      val p = new GeneralPath(e)
      p.transform(r.xForm.asAffine)
      r.g2d.fill(p)
    }
  }

  def renderForceButterfly(plateNumber: Int, r: RenderInfo, startFrame: Int = stanceStart, endFrame: Int = stanceEnd) {
    if (!frameInStance(r.frame)) return
    r.g2d.setColor(butterFlyDraw)
    r.g2d.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    val p = new Path2D.Float()
    val plate = r.c3d.platforms.plates(plateNumber)
    for {
      frame <- startFrame until endFrame
      subframe <- 0 until nSubFrames
      fSample = pfSample(frame, subframe, r.c3d)
      pwa = plate.pwa(fSample)
      force = plate.force(fSample)
    } {
      p.moveTo(pwa.x, pwa.y)
      p.lineTo(force.x * fScale, force.y * fScale)
    }
    p.transform(r.xForm.asAffine)
    r.g2d.draw(p)
  }

  def renderSingleForce(plateNumber: Int, r: RenderInfo) {
    if (!frameInStance(r.frame)) return
    r.g2d.setColor(forceDraw)
    r.g2d.setStroke(new BasicStroke(2.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    val plate = r.c3d.platforms.plates(plateNumber)
    val fSample = pfSample(r.frame, r.subFrame, r.c3d)
    val pwa = plate.pwa(fSample)
    val force = plate.force(fSample)
    val p = arrow(pwa.x, pwa.y, force.x * fScale, force.y * fScale)
    p.transform(r.xForm.asAffine)
    r.g2d.draw(p)
  }

  def renderCOM(r: RenderInfo) {
    val p = Buchner.bodyCOM(static, trial)
    for {
      com1 <- p(r.frame)
      com2 <- p(r.frame + 1)
      v = com1 * (1.0f - r.frameFraction) + com2 * r.frameFraction
    } {
      r.g2d.setColor(comColor)
      r.g2d.setStroke(new BasicStroke(4.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      val pDraw = com(v.x, v.y, comDiam, false)
      val pFill = com(v.x, v.y, comDiam, true)
      pDraw.transform(r.xForm.asAffine)
      pFill.transform(r.xForm.asAffine)
      r.g2d.fill(pFill)
      r.g2d.draw(pDraw)
    }
  }

  /**
   * Creates output directories if they don't already exist.
   */
  private def createOutputDirsIfNecessary() {
    def mkdir(dir: File) = if (!dir.exists()) dir.mkdirs()
    mkdir(outDir)
    mkdir(markerOnlyDir)
    mkdir(comOnlyDir)
  }

  private def frameInStance(frame: Int): Boolean = (frame >= stanceStart) && (frame <= stanceEnd)

  private def pfSample(frame: Int, subFrame: Int, c3d: C3D): Int = {
    val fframe: Float = frame.toFloat + (subFrame.toFloat / nSubFrames.toFloat)
    (fframe * c3d.platforms.rate / c3d.points.rate).toInt
  }

  private def arrow(ox: Float, oy: Float, fx: Float, fy: Float): Path2D = {
    val headHeight = 24
    val headLength = 48
    val dx = fx - ox
    val dy = fy - oy
    val angle = math.atan2(dy, dx)
    val length = math.sqrt(dx * dx + dy * dy)
    val p = new Path2D.Float()
    val xform = new AffineTransform()
    xform.translate(ox, oy)
    xform.rotate(angle)
    p.moveTo(0, 0)
    p.lineTo(length, 0)
    p.moveTo(length - headLength, headHeight / 2)
    p.lineTo(length, 0)
    p.lineTo(length - headLength, -headHeight / 2)
    p.transform(xform)
    p
  }

  private def com(ox: Float, oy: Float, diam: Float, fill: Boolean): Path2D = {
    val r = diam / 2.0f
    val g = new GeneralPath()
    if (fill) {
      g.append(new Arc2D.Float(-r, -r, diam, diam, 0,   90, Arc2D.PIE), false)
      g.append(new Arc2D.Float(-r, -r, diam, diam, 180, 90, Arc2D.PIE), false)
    }
    if (!fill) {
      g.append(new Ellipse2D.Float(-r, -r, diam, diam), false)
    }
    val xform = new AffineTransform()
    xform.translate(ox, oy)
    g.transform(xform)
    g
  }

  /**
   * 2D transformations.
   */
  lazy val xForm: XForm2D = { // default transformation
    val diam = 2.0f * viewRadius
    val uScale = math.min(xRes, yRes) / diam
    XForm2D().scale(1, -1).scale(uScale).translate(viewRadius, -viewRadius).translate(viewXShift, viewYShift)
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
  def colorFromHex(hexString: String): Color = {
    require(hexString.length == 6, "hexString must have six elements")
    val rTxt = hexString.substring(0, 2)
    val gTxt = hexString.substring(2, 4)
    val bTxt = hexString.substring(4, 6)
    new Color(Integer.parseInt(rTxt, 16), Integer.parseInt(gTxt, 16), Integer.parseInt(bTxt, 16))
  }

}
