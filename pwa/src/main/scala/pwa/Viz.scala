package pwa

import c3d._
import scala.collection.immutable._
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.io.File
import java.io.FilenameFilter
import javax.imageio.ImageIO

import core.Direction

object Viz {

  import Geom._
  import PWA._
  
  def make_movie(c3d: C3D, outFile: File, workingDir: File, radius: Float, footfalls: Seq[Footfall], 
      hoofPoints: Seq[Point], segmentCOMs: Seq[Point], bodyCOM: Point, res: Int = 1024)
  {
    // clear out working directory
    clearPNGsFromDirectory(workingDir)
    // skip file if it exists already
    if (outFile.exists) {
      println(s"NOTE: ${outFile.getName} already exists; skipping")
      return
    }
    // dump trial
    dump_trial(c3d, workingDir, radius, footfalls, hoofPoints, segmentCOMs, bodyCOM, res)
    // encode movie
    val indir: String = workingDir.getName
    val outs: String = outFile.getCanonicalFile.toString
    val cmd = Array("ffmpeg", "-pattern_type", "glob", "-i", s"*.jpg", "-c:v", "libx264",
        "-preset", "slow", s"$outs")
    val pb = new ProcessBuilder(cmd: _*).directory(workingDir)
    pb.environment.put("PATH", "/usr/local/bin")
    pb.redirectError(new File(PWA.outDir, "ffmpeg_out.txt"))
    val proc = pb.start()
    if (proc.waitFor() != 0) {
      proc.destroy()
      println("problem executing ffmpeg procedure")
    }
  }
  
  def clearPNGsFromDirectory(dir: File) {
    def fnf: FilenameFilter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.toLowerCase.endsWith(".jpg")
    }
    val pngFiles = dir.listFiles(fnf)
    pngFiles.map(_.delete())
  }
    
  // dump a whole trial as a top-down 2D visualization
  def dump_trial(c3d: C3D, dir: File, radius: Float, footfalls: Seq[Footfall], hoofPoints: Seq[Point],
      segmentCOMs: Seq[Point], bodyCOM: Point, res: Int = 1024) 
  {
    val bodyCOMpoints: IndexedSeq[Vec2D] = bodyCOM.filter(_.isDefined).map(_.get).map(v => Vec2D(v.x, v.y))
    val bodyCOMcircle: Option[Circle] = 
      if (!footfalls.isEmpty && (footfalls.head.direction != Direction.Straight) && !bodyCOMpoints.isEmpty) 
        Some(lsqCircle(bodyCOMpoints)) 
      else 
        None
    val nFrames = c3d.points.totalSamples
    val fFrame: Int = firstFrame(c3d)
    val lFrame: Int = lastFrame(c3d)
    for {
      i <- (fFrame until lFrame).par
      file = new File(dir, f"$i%05d.jpg")
    } dump_frame(c3d, file, radius, i, footfalls, hoofPoints, segmentCOMs, bodyCOM, fFrame, lFrame, bodyCOMcircle, res)
  }

  def firstFrame(c3d: C3D): Int = {
    var i = 0
    while (i < (c3d.points.totalSamples)-1) {
      if (c3d.points.points.exists(_(i).isDefined)) return i
      i = i + 1
    }
    return i
  }
  
  def lastFrame(c3d: C3D): Int = {
    var i = c3d.points.totalSamples - 1
    while (i > 0) {
      if (c3d.points.points.exists(_(i).isDefined)) return i
      i = i - 1
    }
    return i
  }  
  
  // dumps a frame out as a top-down 2D visualization
  def dump_frame(c3d: C3D, file: File, radius: Float, frameNumber: Int, footfalls: Seq[Footfall], 
      hoofPoints: Seq[Point], segmentCOMs: Seq[Point], bodyCOM: Point, firstFrame: Int, lastFrame: Int,
      bodyCOMcircle: Option[Circle],
      res: Int = 1024) 
  {
    val image = new BufferedImage(res, res, BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    // set up antialiasing
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    // set up transform
    val diam = 2.0f * radius
    g.scale(1, -1)
    g.scale(res / diam, res / diam)
    g.translate(radius, -radius)
    g.setColor(Color.WHITE)
    // draw scene elements
    drawForcePlateBoundaries(g, c3d, frameNumber, footfalls)
    drawHitBars(g, c3d, frameNumber, footfalls, firstFrame, lastFrame)
    drawPWAs(g, c3d, frameNumber)
    drawPoints(g, c3d, frameNumber)
    drawMarkers(g, hoofPoints, frameNumber)
    drawSegmentCOMs(g, segmentCOMs, frameNumber)
    if (bodyCOMcircle.isDefined) drawBodyCOM(g, bodyCOM, frameNumber, firstFrame, lastFrame, bodyCOMcircle.get)
    drawTrialInfo(g, c3d.source, bodyCOMcircle)
    g.dispose()
    ImageIO.write(image, "JPG", file)
  }
  
  def drawTrialInfo(g: Graphics2D, trialName: String, bodyCOMCircle: Option[Circle]) {
    val size: Float = 4000.0f
    val margin: Float = 100.0f
    g.setColor(new Color(0.8f, 0.8f, 0.8f))
    g.setFont(g.getFont().deriveFont(150.0f))
    drawTextUpperLeft(g, trialName, -size + margin, size - margin)
    bodyCOMCircle match {
      case Some(c) => {
        val mrad = c.radius / 1000.0f
        drawTextUpperLeft(g, f"COM Radius: $mrad%.2f m", -size + margin, size - margin - 180.0f)
      }
      case None => {}
    }
  }
  
  def drawBodyCOM(g: Graphics2D, bodyCOM: Point, frameNumber: Int, firstFrame: Int, lastFrame: Int, 
      bodyCOMCircle: Circle) {
    
    // COM circle
    val rbx = bodyCOMCircle.origin.x
    val rby = bodyCOMCircle.origin.y
    val rbc = bodyCOMCircle.radius
    val bodyCOMCircleColor: Color = new Color(1.0f, 0.8f, 0.8f).withAlpha(0.2f)
    //val bodyCOMCircleStroke: BasicStroke = new BasicStroke(20.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
    //  80.0f, Array[Float](75.0f, 75.0f), 0.0f)
    val bodyCOMCircleStroke: BasicStroke = new BasicStroke(100.0f)
    g.setColor(bodyCOMCircleColor)
    g.setStroke(bodyCOMCircleStroke)
    g.draw(new Ellipse2D.Float(rbx - rbc, rby - rbc, rbc * 2, rbc * 2))
    
    val bodyCOMColor: Color = new Color(0.6f, 0.6f, 1.0f)
    g.setColor(bodyCOMColor)
    
    // COM path
    val pathDotDiam = 20.0f
    val pathDotRadius = pathDotDiam / 2.0f
    for (frame <- firstFrame until lastFrame) {
      bodyCOM(frame) match {
        case Some(v) => {
          val c = new Ellipse2D.Float(v.x - pathDotRadius, v.y - pathDotRadius, pathDotDiam, pathDotDiam)
          g.fill(c)
        }
        case None => {}
      }
    }
    
    // COM location for this frame
    val bodyCOMDiam = 160.0f
    g.setStroke(new BasicStroke(10.0f)) // stroke width
    val r = bodyCOMDiam / 2.0f
    bodyCOM(frameNumber) match {
      case Some(v) => {
        val c = new Ellipse2D.Float(v.x - r, v.y - r, bodyCOMDiam, bodyCOMDiam)
        g.draw(c)
        val a1 = new Arc2D.Float(v.x - r, v.y - r, bodyCOMDiam, bodyCOMDiam,   0, 90, Arc2D.PIE)
        val a2 = new Arc2D.Float(v.x - r, v.y - r, bodyCOMDiam, bodyCOMDiam, 180, 90, Arc2D.PIE)
        g.fill(a1)
        g.fill(a2)
      }
      case None => {}
    }
  }
  
  def drawSegmentCOMs(g: Graphics2D, coms: Seq[Point], frameNumber: Int) {
    val segmentCOMDiam = 120.0f
    val segmentCOMColor: Color = Color.GREEN.withAlpha(0.5f)
    g.setColor(segmentCOMColor)
    g.setStroke(new BasicStroke(10.0f)) // stroke width
    val r = segmentCOMDiam / 2.0f
    for {
      point <- coms
      optVec: Option[Vec3D] = point(frameNumber)
      if (optVec.isDefined)
      v: Vec3D = optVec.get
    } {
      val c = new Ellipse2D.Float(v.x - r, v.y - r, segmentCOMDiam, segmentCOMDiam)
      g.draw(c)
      val a1 = new Arc2D.Float(v.x - r, v.y - r, segmentCOMDiam, segmentCOMDiam,   0, 90, Arc2D.PIE)
      val a2 = new Arc2D.Float(v.x - r, v.y - r, segmentCOMDiam, segmentCOMDiam, 180, 90, Arc2D.PIE)
      g.fill(a1)
      g.fill(a2)
    }
  }
  
  def drawMarkers(g: Graphics2D, hoofPoints: Seq[Point], frameNumber: Int) {
    val markerdiam = 35.0f
    val r = markerdiam / 2.0f
    g.setColor(Color.ORANGE)
    for {
      point <- hoofPoints
      optVec: Option[Vec3D] = point(frameNumber)
      if (optVec.isDefined)
      v: Vec3D = optVec.get
    } {
      val e = new Ellipse2D.Float(v.x - r, v.y - r, markerdiam, markerdiam)
      g.fill(e)      
    }
  }
  
  def drawPoints(g: Graphics2D, c3d: C3D, frameNumber: Int) {
    val markerdiam = 30.0f
    val r = markerdiam / 2.0f
    g.setColor(Color.WHITE)
    for {
      point <- c3d.points.points
      optVec: Option[Vec3D] = point(frameNumber)
      if (optVec.isDefined)
      v: Vec3D = optVec.get
    } {
      val e = new Ellipse2D.Float(v.x - r, v.y - r, markerdiam, markerdiam)
      g.fill(e)
    }
  }
  
  def drawPWAs(g: Graphics2D, c3d: C3D, frameNumber: Int) {
    val forceThreshold = PWA.forceThreshold
    val diam = 200.0f
    val r = diam / 2.0f
    val dd: Float = (r / math.sqrt(2.0)).toFloat
    val i: Int = (frameNumber * c3d.platforms.rate / c3d.points.rate).toInt
    g.setColor(Color.CYAN)
    g.setStroke(new BasicStroke(10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    for {
      plate <- c3d.platforms.plates
      fMag: Float = plate.force(i).mag
      if (fMag > forceThreshold)
      pwa: Vec3D = plate.pwa(i)
      x: Float = pwa.x
      y: Float = pwa.y
    } {
      val e = new Ellipse2D.Float(x - r, y - r, diam, diam)
      val p = new Path2D.Float()
      p.moveTo(x - dd, y - dd)
      p.lineTo(x + dd, y + dd)
      p.moveTo(x + dd, y - dd)
      p.lineTo(x - dd, y + dd)
      g.draw(e)
      g.draw(p)
    }
  }
  
  def drawForcePlateBoundaries(g: Graphics2D, c3d: C3D, frameNumber: Int, footfalls: Seq[Footfall]) {
    for {
      plate <- c3d.platforms.plates
      corners: IndexedSeq[Vec3D] = plate.corners
    } {
      assert(corners.length == 4, "force plate must have four corners for drawing")
      def footfallContainsFrameNumber(footfall: Footfall): Boolean = {
        (frameNumber >= footfall.interval.on) && (frameNumber <= footfall.interval.off)
      }
      val plateNumber = c3d.platforms.plates.indexOf(plate) + 1
      // draw path
      val p = new Path2D.Float()
      p.moveTo(corners(0).x, corners(0).y)
      p.lineTo(corners(1).x, corners(1).y)
      p.lineTo(corners(2).x, corners(2).y)
      p.lineTo(corners(3).x, corners(3).y)
      p.closePath()
      val strikes: Seq[Footfall] = footfalls.filter(footfallContainsFrameNumber(_))
      val plateColor = if (strikes.find(_.plateNumber == plateNumber).isDefined) {
        Color.YELLOW.darker.withAlpha(0.9f)
      } else {
        Color.YELLOW.darker.withAlpha(0.5f)
      }
      g.setColor(plateColor)
      g.fill(p)
      g.setColor(Color.YELLOW.darker)
      g.setStroke(new BasicStroke(5.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      g.draw(p)
      // draw number
      val cx = corners.map(_.x).sum / 4.0f
      val cy = corners.map(_.y).sum / 4.0f
      g.setFont(g.getFont().deriveFont(200.0f))
      drawTextCentered(g, s"$plateNumber", cx, cy)
    }
  }
  
  def drawHitBars(g: Graphics2D, c3d: C3D, frameNumber: Int, footfalls: Seq[Footfall], firstFrame: Int, 
      lastFrame: Int) {
    val size: Float = 4000.0f
    val lrMargin: Float = 200.0f
    val bottomMargin: Float = 100.0f
    val barHeight: Float = 200.0f
    val barWidth: Float = 2 * (size - lrMargin)
    val nFrames: Float = lastFrame - firstFrame
    val frameHighlightColor: Color = Color.RED.withAlpha(0.5f)
    def drawBar(plateNumber: Int, y: Float) {
      
      def frameToX(frame: Int): Float = (-size+lrMargin) + ((frame - firstFrame) / nFrames) * barWidth
      def footfallContainsFrameNumber(footfall: Footfall): Boolean = {
        (frameNumber >= footfall.interval.on) && (frameNumber <= footfall.interval.off)
      }
      
      // hits
      val strikes: Seq[Footfall] = footfalls.filter(footfallContainsFrameNumber(_))
      g.setFont(g.getFont().deriveFont(150.0f))
      for {
        ff <- footfalls
        if (ff.plateNumber == (plateNumber + 1))
        x1 = frameToX(ff.interval.on)
        x2 = frameToX(ff.interval.off)
        width = x2 - x1
      } {
        val hitColor: Color = if (footfallContainsFrameNumber(ff)) {
          Color.YELLOW.darker.withAlpha(0.9f)
        } else {
          Color.YELLOW.darker.withAlpha(0.5f)
        }
        g.setColor(hitColor)
        g.fill(new Rectangle2D.Float(x1, y, width, barHeight))
        g.setColor(Color.BLACK)
        drawTextCentered(g, ff.limb.toString, x1 + (width / 2.0f), y + (barHeight / 2.0f))
      }
      
      // current frame
      val x0 = frameToX(frameNumber)
      val w0 = barWidth / nFrames
      g.setColor(frameHighlightColor)
      g.fill(new Rectangle2D.Float(x0, y, w0, barHeight))
      
      // border
      g.setColor(Color.YELLOW.darker)
      g.setStroke(new BasicStroke(10.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      g.draw(new Rectangle2D.Float(-size+lrMargin, y, barWidth, barHeight))
      
      // frame boundaries
      g.setColor(Color.YELLOW.darker.withAlpha(0.25f))
      g.setStroke(new BasicStroke(7.5f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_BEVEL))
      for (frame <- firstFrame until lastFrame) {
        val x = frameToX(frame)
        g.draw(new Line2D.Float(x, y, x, y+barHeight))
      }

    }
    for (i <- 0 to 5) drawBar(i, -size + bottomMargin + (barHeight * (5-i)))
  }
  
  def drawTextCentered(g: Graphics2D, text: String, x: Float, y: Float) {
    val oTransform = g.getTransform()
    val fm = g.getFontMetrics()
    val bounds = fm.getStringBounds(text, g)
    val dx = bounds.getWidth / 2
    val dy = bounds.getHeight / 2 - fm.getDescent
    g.translate((x - dx).toFloat, (y - dy).toFloat)
    g.scale(1, -1)
    g.drawString(text, 0, 0)
    g.setTransform(oTransform)
  }
  
  def drawTextUpperLeft(g: Graphics2D, text: String, x: Float, y: Float) {
    val oTransform = g.getTransform()
    val fm = g.getFontMetrics()
    val bounds = fm.getStringBounds(text, g)
    g.translate(x, y - bounds.getHeight)
    g.scale(1, -1)
    g.drawString(text, 0, 0)
    g.setTransform(oTransform)
  }
  
  implicit class RichColor(c: Color) {
    def r: Int = c.getRed
    def g: Int = c.getGreen
    def b: Int = c.getBlue
    def withAlpha(alpha: Float) = new Color(r, g, b, (alpha * 255).toInt)
  }
  
}