package circlestudy.prototyping

import circlestudy.trials.{Trial, TrialDatabase, TRCTrialDatabase}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.awt.Graphics2D
import scala.collection.immutable._
import circlestudy.render.CircleRender

object CircleCenterPrototype extends App {
  
  println("====================================")
  println("Circle Center Finding Prototype Code")
  println("====================================")
  
  // Load a trial to play with
  val trial: Trial = {
    import TrialDatabase.Metadata
    import TrialDatabase.Gait
    import TrialDatabase.Curvature.Circle
    import TrialDatabase.Curvature.Direction.Left
    
    TRCTrialDatabase.getTrial(Metadata(3, Circle(Left), Gait.Trot, 3)).getOrElse(
      sys.error("Could not load example trial.")
    )
  }
  
  // Create a BufferedImage to render the trial data
  val imageSize: Int = 1024
  val bufferedImage: BufferedImage = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_RGB)
  val g2d: Graphics2D = bufferedImage.getGraphics().asInstanceOf[Graphics2D]
  
  // Render the marker trails onto the image
  CircleRender.render2DMarkerTrails(trial, g2d, bufferedImage.getWidth, bufferedImage.getHeight,
      Some(Set("T6")))
  
  // Save the BufferedImage as a PNG
  g2d.dispose()
  ImageIO.write(bufferedImage, "PNG", new File("output/prototyping/circlecenterprototype.png"))
  
}