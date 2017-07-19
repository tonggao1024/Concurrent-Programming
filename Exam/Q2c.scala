import io.threadcso._ 
import scala.io.StdIn.readLine

object Q2c{
	val W = 500 // Num of boids
	val r = scala.util.Random
	val allBoids = new Array[Boid](W) // Array of boids

	//Initialization
	for (i <- 0 until W){
	  val myBoid = new Boid
	  myBoid.init(r)
	  allBoids(i) = myBoid
	}

	//BoidParams.Coh = -1
	
	val display = new Display(allBoids)// Display
	val barrier = new Barrier(W)// Barrier

	//Worker
	def Worker(id: Int) = proc{
	    while(true){
		    if(id == 0) display.draw //the first boid is responsible for drawing the grid
		    sleep(BoidParams.Rate)
		    // compute new state
		    val nextState = allBoids(id).newState(allBoids.toIterable)
		    // wait for all boids finish the local computation
		    barrier.sync
		    // update the array
		    allBoids(id).setState(nextState)
		    // wait for all boids finish the update
		    barrier.sync
	    }
	}

	//User
	def User = proc{
		while(true){
			val x = Console.readLine 
			val array = x.split(" ")
			array(0) match {
				case "Rate" => { BoidParams.Rate = array(1).toInt }
				case "Coh" => { BoidParams.Coh = array(1).toDouble }
				case "Sep" => { BoidParams.Sep = array(1).toDouble }
				case "Align" => { BoidParams.Align = array(1).toDouble }
				case "maxSpeed" => { BoidParams.maxSpeed = array(1).toDouble }
				case "minSpeed" => { BoidParams.minSpeed = array(1).toDouble }
				case "breadth" => { BoidParams.breadth = array(1).toDouble }
				case "range" => { BoidParams.range = array(1).toDouble }
				case "drag" => { BoidParams.drag = array(1).toDouble }
				case _ => {}				  
			}
		}
	}

	//System
	def System = (||(for(i <- 0 until W) yield Worker(i))) || User

	//Main
	def main(args: Array[String]) = System()
}
