package circlestudy.render

import scala.collection.immutable._
import circlestudy.trials.Trial.MarkerWithGaps
import java.awt.Graphics2D
import circlestudy.Vec3
import java.awt.geom.Path2D
import java.awt.geom.Ellipse2D
import java.awt.BasicStroke

object CircleRender {

  /** 
   * Plots trails of markers in 2D. 
   */
  def markerTrails(g2d: Graphics2D)(markers: Set[MarkerWithGaps]) {
    
    // Finds the sequences of contiguous coordinates within an IndexedSeq of optionally-defined coordinates.
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

    // plot markers
    for (marker <- markers) {
      for (contigCoords <- contiguousCoordinates(marker.co)) {
        val path = new Path2D.Double()
        path.moveTo(contigCoords.head.x, contigCoords.head.y)
        for (co <- contigCoords.tail) path.lineTo(co.x, co.y)
        g2d.draw(path)
      }
    }
  }
  
  /**
   * Renders fitted circles in 2D.
   */
  def fittedCircles
    (g2d: Graphics2D, centerHeight: Double = 100, centerLineWidth: Double = 10, circleLineWidth: Double = 30)
    (xc: Double, yc: Double, rs: IndexedSeq[Double]) 
  {
    // cross and circle at the center
    val cr = centerHeight / 2.0
    g2d.setStroke(new BasicStroke(centerLineWidth.toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    val path = new Path2D.Double()
    path.moveTo(xc - cr, yc)
    path.lineTo(xc + cr, yc)
    path.moveTo(xc, yc - cr)
    path.lineTo(xc, yc + cr)
    g2d.draw(path)
    // circles
    g2d.setStroke(new BasicStroke(circleLineWidth.toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    for (r <- rs) g2d.draw(new Ellipse2D.Double(xc - r, yc - r, 2.0 * r, 2.0 * r))
  }
    
}
