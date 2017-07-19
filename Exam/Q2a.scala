import io.threadcso._ 

object Q2a{
	val W = 200 // num of boids
	val r = scala.util.Random
	val allBoids = new Array[Boid](W) // array of boids

	//Initialization
	for (i <- 0 until W){
	  val myBoid = new Boid
	  myBoid.init(r)
	  allBoids(i) = myBoid
	}
	
	val display = new Display(allBoids)// Display
	val barrier = new Barrier(W)// Barrier

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

	def System = ||(for(i <- 0 until W) yield Worker(i))
	def main(args: Array[String]) = System()
}
