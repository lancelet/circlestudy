package circlestudy.render

import circlestudy.trials.Trial
import java.awt.Graphics2D
import scala.collection.immutable._
import circlestudy.Vec3
import java.awt.geom.Path2D
import java.awt.Color
import java.awt.RenderingHints
import circlestudy.Bound2
import circlestudy.Bound3
import circlestudy.trials.Trial.MarkerWithGaps
import java.awt.geom.Ellipse2D

object CircleRender {

  /**
   * Sets the viewport visible in a Graphics2D object to a specified bounding box.
   * 
   * This method assumes that the Graphics2D object begins with a coordinate system that has zeroes at the top left,
   * and extends to the specified width and height.
   */
  def setViewport(g2d: Graphics2D, width: Int, height: Int, bound: Bound2) {
    // flip the y-axis first, and move the origin to the lower-left
    g2d.scale(1.0, -1.0)
    g2d.translate(0.0, -height)
    
    // scale so that width and height match
    g2d.scale(width.toDouble / bound.width, height.toDouble / bound.height)
    
    // transform the origin to the minimum coordinate of the bounding box
    g2d.translate(-bound.min.x, -bound.min.y)
  }

  /**
   * Expands a Bound2 in either its width or height so that it conforms to a given aspect ratio.
   */
  def conformBoundToAspect(bound: Bound2, width: Int, height: Int): Bound2 = {
    val frameAspect = width.toDouble / height.toDouble
    val boundAspect = bound.width / bound.height
    
    if (boundAspect > frameAspect) {
      //val f = boundAspect / frameAspect
      val f = boundAspect - frameAspect
      val shift = 0.5 * bound.height * f
      Bound2(bound.min.x, bound.max.x, bound.min.y - shift, bound.max.y + shift)
    } else {
      //val f = frameAspect / boundAspect
      val f = frameAspect - boundAspect
      val shift = 0.5 * bound.width * f
      Bound2(bound.min.x - shift, bound.max.x + shift, bound.min.y, bound.max.y)
    }
  }
  
  /**
   * Expands the margins of a Bound2 by a given fraction.
   * 
   * For example, a fraction of 0.1 adds 0.1 to the width and height of the Bound2.  The center of the Bound2 is
   * retained.
   */
  def expandMargins(bound: Bound2, fraction: Double): Bound2 = {
    val dx = 0.5 * fraction * bound.width
    val dy = 0.5 * fraction * bound.height
    Bound2(bound.min.x - dx, bound.max.x + dx, bound.min.y - dy, bound.max.y + dy)
  }
  
  /**
   * Renders the trails of markers in 2D.
   */
  def render2DMarkerTrails(trial: Trial, g2d: Graphics2D, width: Int, height: Int,
    namesOfMarkersToPlot: Option[Set[String]] = None) {

    /** Finds the sequences of contiguous coordinates within an IndexedSeq of optionally-defined coordinates. */
    def contiguousCoordinates(coords: IndexedSeq[Option[Vec3]]): Seq[IndexedSeq[Vec3]] = {
      def accumulateCoordinates(coords: IndexedSeq[Option[Vec3]], coordsSeq: Seq[IndexedSeq[Vec3]]):
    	Seq[IndexedSeq[Vec3]] = {
        val (gap, remainder) = coords.span(!_.isDefined)
        val (contig, remainder2) = remainder.span(_.isDefined)
        if (remainder2.isEmpty) {
          coordsSeq
        } else {
          val newCoordsSeq = coordsSeq :+ contig.map(_.get)
          accumulateCoordinates(remainder2, newCoordsSeq)
        }
      }
      accumulateCoordinates(coords, Seq.empty[IndexedSeq[Vec3]])
    }
    
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // set the viewport to view all of the trial
    val b3d: Bound3 = trial.bounds
    val b2d: Bound2 = expandMargins(Bound2(b3d.min.x, b3d.max.x, b3d.min.y, b3d.max.y), 0.1)
    val b2dConformed: Bound2 = conformBoundToAspect(b2d, width, height)
    setViewport(g2d, width, height, b2dConformed)
    
    // fetch the markers to render
    val markersToPlot: Seq[MarkerWithGaps] = namesOfMarkersToPlot map { nameSet =>
      trial.markers.filter(_.exists).filter(nameSet contains _.name)
    } getOrElse trial.markers.filter(_.exists)
    
    // render marker paths
    g2d.setColor(TrekColors.Orange)    
    for (marker <- markersToPlot) {
      for (contigCoords <- contiguousCoordinates(marker.co)) {
        val path = new Path2D.Double()
        path.moveTo(contigCoords.head.x, contigCoords.head.y)
        for (co <- contigCoords.tail) {
          path.lineTo(co.x, co.y)
        }
        g2d.draw(path)
      }
    }
  }

  /**
   * Renders fitted circles in 2D.  (Assumes coordinate system of g2d is already set up.)
   */
  def renderCircles(xc: Double, yc: Double, rs: IndexedSeq[Double], g2d: Graphics2D) {
    g2d.setColor(TrekColors.Blue)
    for (r <- rs) {
      g2d.draw(new Ellipse2D.Double(xc - r, yc - r, 2.0*r, 2.0*r))
    }
  }
  
}