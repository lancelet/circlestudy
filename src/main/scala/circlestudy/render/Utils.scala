package circlestudy.render

import java.awt.{Dimension, Graphics2D}
import math.abs
import circlestudy.Bound2

object Utils {

  /**
   * Sets the viewport on a Graphics2D object to accommodate provided 2D bounds.
   * 
   * The default coordinate system for a Graphics2D object has an origin at the top left, with x coordinates
   * increasing to the right, and y coordinates increasing downward.  This method sets the transformation to
   * accommodate the bounding box provided.
   * 
   * @param g2d graphics context
   * @param size dimensions of the drawing area (or the image into which the drawing will be performed)
   * @param bound bounding box to accommodate
   * @param retainBoundsAspect retain a square aspect ratio
   * @param marginFraction fraction by which to increase the bounding box
   */
  def setViewport(g2d: Graphics2D, size: Dimension, bound: Bound2, 
    retainBoundsAspect: Boolean = true, marginFraction: Double = 0.0) 
  {
    // Expand bounding box by the specified margin, and conform its aspect ratio to the frame if necessary
    val bExpanded = expandMargins(bound, marginFraction)
    val bConformed = if (retainBoundsAspect) conformBoundToSize(bExpanded, size) else bExpanded
    
    // Set the viewport using standard transformations
    g2d.scale(1.0, -1.0)
    g2d.translate(0.0, -size.getHeight)
    g2d.scale(size.getWidth / bConformed.width, size.getHeight / bConformed.height)
    g2d.translate(-bConformed.min.x, -bConformed.min.y)
  }

  /** Expands the margins of a bounding box by a given fraction. */
  private [this] def expandMargins(bound: Bound2, fraction: Double): Bound2 = if (fraction != 0.0) {
    val dx = 0.5 * fraction * bound.width
    val dy = 0.5 * fraction * bound.height
    Bound2(bound.min.x - dx, bound.max.x + dx, bound.min.y - dy, bound.max.y + dy)
  } else bound
  
  /** Expands a Bound2 in either width or height, so that its aspect ratio conforms to a passed-in Dimension. */
  private [this] def conformBoundToSize(bound: Bound2, size: Dimension): Bound2 = {
    val frameAspect = size.getWidth / size.getHeight
    val boundAspect = bound.width / bound.height
    
    val f = abs(frameAspect - boundAspect)
    if (boundAspect > frameAspect) {
      val d = 0.5 * bound.height * f
      Bound2(bound.min.x, bound.max.x, bound.min.y - d, bound.max.y + d)
    } else {
      val d = 0.5 * bound.width * f
      Bound2(bound.min.x - d, bound.max.x + d, bound.min.y, bound.max.y)
    }
  }
  
}
