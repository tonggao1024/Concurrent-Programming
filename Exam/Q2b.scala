import io.threadcso._

object Q2b{
	//task
	type taskToWorker = (Array[Boid], Int) // worker id
	type resultToController = (Boid.State, Int) // worker id

	//worker
	def Worker(fromController: ?[taskToWorker], toController: ![resultToController]) = proc{
		repeat{
			val (allBoids, id) = fromController?()
			val nextState = allBoids(id).newState(allBoids)
			toController!(nextState, id)
		}
	}

	//controller
	def Controller(nworkers: Int, toWorkers: ![taskToWorker], fromWorkers: ?[resultToController]) = proc{
		val allBoids = new Array[Boid](nworkers) //array of boids
		val display = new Display(allBoids) //display
		
		//Initialization
		for (i <- 0 until nworkers){
		  val myBoid = new Boid
		  val r = scala.util.Random
		  myBoid.init(r)
		  allBoids(i) = myBoid
		}

		repeat{
			display.draw

			// send task to worker
			for(i <- 0 until nworkers){
				toWorkers!(allBoids,i)
			}

			// receive the results and update the array
			for(i <- 0 until nworkers){
				val (nextState, id) = fromWorkers?()
				allBoids(id).setState(nextState)
			}
		}
	}

	//system
	def System: PROC = {	
		val toWorkers = N2N[taskToWorker](1,0,"toWorkers") // N2N channel
		val toController = N2N[resultToController](0,1,"toController") // N2N channel
		val nworkers = 200 // number of workers

		val Workers = ||(for(i <- 0 until nworkers) yield Worker(toWorkers, toController))

		(Workers || Controller(nworkers, toWorkers, toController))
	}

	//Main
	def main(args: Array[String]) = System()
}
