package circlestudy.prototyping

import circlestudy.trials.{Trial, TrialDatabase, TRCTrialDatabase}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.awt.Graphics2D
import scala.collection.immutable._
import circlestudy.trials.Trial.MarkerWithGaps
import circlestudy.render.Utils
import circlestudy.render.OldCircleRender
import circlestudy.optim.MultiCircleFit
import circlestudy.optim.MarkerWithGapsAsMCFMarker
import java.awt.Dimension
import java.awt.geom.AffineTransform
import circlestudy.Bound2
import java.awt.RenderingHints
import circlestudy.render.CircleRender
import circlestudy.render.TrekColors
import java.awt.BasicStroke
import java.awt.Font
import java.awt.geom.Rectangle2D
import java.awt.font.TextLayout
import java.awt.Color

object CircleCenterPrototype extends App {
  
  println("====================================")
  println("Circle Center Finding Prototype Code")
  println("====================================")
  
  // Load a trial to play with (Horse3_circle_left_trot_3.trc)
  val trial: Trial = {
    import TrialDatabase.Metadata
    import TrialDatabase.Gait
    import TrialDatabase.Curvature.Circle
    import TrialDatabase.Curvature.Direction.Left
    
    TRCTrialDatabase.getTrial(Metadata(3, Circle(Left), Gait.Trot, 3)).getOrElse(
      sys.error("Could not load example trial.")
    )
  }
  
  // Constants
  val dir: File = new File("output/prototyping/circlecenter/")  // directory to save files
  val imageSize: Dimension = new Dimension(800, 800)
  
  /** Creates a new image, returning the image, a graphics context and its original transform. */
  private [this] def createImage(): (BufferedImage, Graphics2D, AffineTransform) = {
    val image = new BufferedImage(imageSize.getWidth.toInt, imageSize.getHeight.toInt, BufferedImage.TYPE_INT_RGB)
    val g2d = image.getGraphics.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val idTransform = g2d.getTransform
    (image, g2d, idTransform)
  }
  
  /**
   * Plots an image of the best-fit circle for *just* the T6 marker.
   */
  def plotT6MarkerCircle() {
	// create image
    val (image, g2d, idTransform) = createImage()

    // find best-fit circle for T6 marker
    val t6: MarkerWithGaps = trial.markers.find(_.name == "T6").get
    val (xc, yc, rs) = MultiCircleFit.fit(IndexedSeq(MarkerWithGapsAsMCFMarker(t6)), IndexedSeq(1.0))
    val r = rs(0)

    // set viewport for the scene
    val bound = t6.bounds.get.toBound2.addBound2(Bound2(xc - r, xc + r, yc - r, yc + r))
    Utils.setViewport(g2d, imageSize, bound, true, 0.2)

    // render the fitted circle for T6
    g2d.setColor(TrekColors.Purple)
    CircleRender.fittedCircles(g2d)(xc, yc, rs)
        
    // render the marker traces for T6
    g2d.setColor(TrekColors.Orange)
    g2d.setStroke(new BasicStroke(20.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    CircleRender.markerTrails(g2d)(Set(t6))
    
    // draw text on the image
    g2d.setTransform(idTransform)
    drawText(g2d)("HORSE3_CIRCLE_LEFT_TROT_3", "MARKER: T6", "22 APR 2013")    
    
    // save image
    g2d.dispose()
    ImageIO.write(image, "PNG", new File(dir, "T6-circle.png"))
  }

  def plotSingleFitAllMarkerCircles() {
    // create image
    val (image, g2d, idTransform) = createImage()
     
    // find best fit for all markers
    val markers = trial.markers
    val weights = IndexedSeq.fill(markers.length)(1.0)
    val (xc, yc, rs) = MultiCircleFit.fit(markers.map(MarkerWithGapsAsMCFMarker(_, 10)), weights)
    
    // set viewport
    val bound: Bound2 = {
      val circleBounds = rs map { r => Bound2(xc - r, xc + r, yc - r, yc + r) }
      circleBounds.fold(trial.bounds.toBound2)(_ addBound2 _)
    }
    Utils.setViewport(g2d, imageSize, bound, true, 0.2)
 
    // render fitted circles
    val p = TrekColors.Purple
    g2d.setColor(new Color(p.getRed, p.getGreen, p.getBlue, 128))
    CircleRender.fittedCircles(g2d, 100, 10, 20)(xc, yc, rs)    
    
    // render marker traces
    g2d.setColor(TrekColors.Orange)
    g2d.setStroke(new BasicStroke(20.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    CircleRender.markerTrails(g2d)(markers.toSet)
     
    // draw text on the image
    g2d.setTransform(idTransform)
    drawText(g2d)("HORSE3_CIRCLE_LEFT_TROT_3", "MARKERS: ALL: SINGLE CENTER", "22 APR 2013")
    
    // save image
    g2d.dispose()
    ImageIO.write(image, "PNG", new File(dir, "single-fit-all-marker-circles.png"))
  }
  
  def plotMultipleFitAllMarkerCircles() {
    // create image
    val (image, g2d, idTransform) = createImage()
    
    // find best fit for each marker
    val markers = trial.markers
    val markerAndFit: IndexedSeq[(MarkerWithGaps, Double, Double, Double)] = for (marker <- markers) yield {
      val (xc, yc, rs) = MultiCircleFit.fit(IndexedSeq(MarkerWithGapsAsMCFMarker(marker)), IndexedSeq(1.0))
      (marker, xc, yc, rs(0))
    }
    
    // set viewport
    val bound: Bound2 = {
      val circleBounds = markerAndFit map { mf =>
        val (marker, xc, yc, r) = mf
      	Bound2(xc - r, xc + r, yc - r, yc + r)
      }
      circleBounds.fold(trial.bounds.toBound2)(_ addBound2 _)
    }
    Utils.setViewport(g2d, imageSize, bound, true, 0.2)

    // render fitted circles
    val p = TrekColors.Purple
    g2d.setColor(new Color(p.getRed, p.getGreen, p.getBlue, 128))
    for ((marker, xc, yc, r) <- markerAndFit) {
      CircleRender.fittedCircles(g2d, 100, 10, 20)(xc, yc, IndexedSeq(r))
    }    
    
    // render marker traces
    g2d.setColor(TrekColors.Orange)
    g2d.setStroke(new BasicStroke(20.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    CircleRender.markerTrails(g2d)(markers.toSet)

    // draw text on the image
    g2d.setTransform(idTransform)
    g2d.setColor(TrekColors.Purple)
    drawText(g2d)("HORSE3_CIRCLE_LEFT_TROT_3", "MARKERS: ALL: MULTIPLE CENTERS", "22 APR 2013")
    
    // save image
    g2d.dispose()
    ImageIO.write(image, "PNG", new File(dir, "multiple-fit-all-marker-circles.png"))
  }
  
  def plotMultipleFitSpineMarkerCircles() {
    // create image
    val (image, g2d, idTransform) = createImage()

    // find best fit for each marker
    val markerNames = Set("T6", "T8", "T12", "T16", "L3", "L6", "S2", "S4")
    val markers = trial.markers.filter(markerNames contains _.name)
    val markerAndFit: IndexedSeq[(MarkerWithGaps, Double, Double, Double)] = for (marker <- markers) yield {
      val (xc, yc, rs) = MultiCircleFit.fit(IndexedSeq(MarkerWithGapsAsMCFMarker(marker)), IndexedSeq(1.0))
      (marker, xc, yc, rs(0))
    }
    
    // set viewport
    val bound: Bound2 = {
      val circleBounds = markerAndFit map { mf =>
        val (marker, xc, yc, r) = mf
      	Bound2(xc - r, xc + r, yc - r, yc + r)
      }
      circleBounds.fold(trial.bounds.toBound2)(_ addBound2 _)
    }
    Utils.setViewport(g2d, imageSize, bound, true, 0.2)

    // render fitted circles
    val p = TrekColors.Purple
    g2d.setColor(new Color(p.getRed, p.getGreen, p.getBlue, 128))
    for ((marker, xc, yc, r) <- markerAndFit) {
      CircleRender.fittedCircles(g2d, 100, 10, 20)(xc, yc, IndexedSeq(r))
    }    
    
    // render marker traces
    g2d.setColor(TrekColors.Orange)
    g2d.setStroke(new BasicStroke(20.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    CircleRender.markerTrails(g2d)(markers.toSet)    
    
    // draw text on the image
    g2d.setTransform(idTransform)
    g2d.setColor(TrekColors.Purple)
    drawText(g2d)("HORSE3_CIRCLE_LEFT_TROT_3", "MARKERS: T6, T8, T12, T16, L3, L6, S2, S4", "22 APR 2013")    
    
    // save image
    g2d.dispose()
    ImageIO.write(image, "PNG", new File(dir, "multiple-fit-spine-marker-circles.png"))
  }
  
  def drawText(g2d: Graphics2D, txtBorder: Double = 6)(topLeft: String, topRight: String, bottomRight: String) {
    g2d.setColor(TrekColors.Blue)
    val font: Font = new Font("Swiss911 UCm BT", Font.PLAIN, 24)
    g2d.setFont(font)
    val frc = g2d.getFontRenderContext()
    
    // top-left text
    val tl = new TextLayout(topLeft, font, frc)
    val tld = tl.getBounds()
    tl.draw(g2d, 0 + txtBorder.toFloat, tld.getHeight.toFloat + txtBorder.toFloat)
    
    // top-right text
    val tr = new TextLayout(topRight, font, frc)
    val trd = tr.getBounds()
    tr.draw(g2d, (imageSize.getWidth - trd.getWidth - txtBorder).toFloat, (trd.getHeight + txtBorder).toFloat)
    
    // bottom-right text
    val br = new TextLayout(bottomRight, font, frc)
    val brd = br.getBounds()
    br.draw(g2d, (imageSize.getWidth - brd.getWidth - txtBorder).toFloat,
        (imageSize.getHeight - txtBorder).toFloat)
  }

  plotT6MarkerCircle()
  plotSingleFitAllMarkerCircles()
  plotMultipleFitAllMarkerCircles()
  plotMultipleFitSpineMarkerCircles()
}