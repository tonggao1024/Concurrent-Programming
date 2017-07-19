import java.awt._;
import javax.swing.{JFrame,JComponent}
import scala.collection.mutable.Set
import io.threadcso._


class Display(boids: Iterable[Boid]) extends JFrame {

  // Constants
  private val gridSize = 1.0       // pixels per unit length 

  // dimensions of triangle representing boid (pixels) 
  private val boidLength = 15.0   // length of triangle
  private val boidBase   = 5.0    // half width of base of triangle
  
  // arena size (pixels)
  private val bwidth  = (BBox.xSize*gridSize).toInt
  private val bheight = (BBox.ySize*gridSize).toInt

  // Set up the display
  val board = new Board()
  board.setPreferredSize(new Dimension(bwidth, bheight))
  
  this.getContentPane.add(board, "Center");
  this.pack()
  this.setVisible(true);
  
  // To enable synchronization of the end of the off-GUI-thread draw method
  // with the end of the on-GUI-thread paint method, so a process using the 
  // draw method will /never/ ``overtake'' the JAVA GUI process
  private [this] val syncsema = BooleanSemaphore(available=false)

  // Redraw the display from the current state of boids
  // Called /off/ the GUI thread
  def draw = 
  { board.paintImmediately(0, 0, bwidth, bheight); 
    syncsema.down // await completion of painting in Java GUI thread
  }
  
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setTitle("Boids")

  class Board extends JComponent {
    val boidColor = Color.blue
    
    // Called /on/ the GUI thread
    override def paint(g: Graphics) = {
      g.setColor(boidColor)
      for (b <- boids) paintBoid(b, g)
      syncsema.up // signal completion of painting
    }

    // Display a boid
    private def paintBoid(boid: Boid, g: Graphics) = {    
      // Calculate coordinates of triangle representing boid
      val xPoints = new Array[Double](3)
      val yPoints = new Array[Double](3) 
      val sin = Math.sin(boid.direction)
      val cos = Math.cos(boid.direction)
      xPoints(0) = boid.x + boidLength * sin; 
      yPoints(0) = boid.y + boidLength * cos;
      xPoints(1) = boid.x - boidBase * cos; 
      yPoints(1) = boid.y + boidBase * sin;
      xPoints(2) = boid.x + boidBase * cos; 
      yPoints(2) = boid.y - boidBase * sin;
      g.fillPolygon(xPoints.map(x => x.toInt), yPoints.map(y => y.toInt), 3);
    }
  }
}










