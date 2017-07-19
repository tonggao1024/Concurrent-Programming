import io.threadcso._
import io.threadcso.semaphore._

trait ZipSlot[T,U] {
	/** Protocol for writing type T */
	def write0(v0: T): Unit
	/** Protocol for writing type U */
	def write1(v1: U): Unit
	/** Protocol for read */
	def read: (T, U)
}

class MonitorZipSlotBounded[T,U] extends ZipSlot[T,U] {
	private val monitor = new Monitor
	private val canRead = monitor.newCondition 
	private val canWrite = monitor.newCondition

	//flag
	private var finishW0 = false
	private var finishW1 = false
	private var readyToRead = false
	private var termination = false

	//variables
	private var value0: T = _
	private var value1: U = _

	//method
	def write0(v0: T) = monitor withLock{
		while(finishW0) canWrite.await 
		if(termination) { println("A stop"); stop}// check termination
		value0 = v0 
		finishW0 = true 
		println("A end write" + ' ' + value0) // Testing
		if(finishW0 && finishW1) { 
			readyToRead = true
			canRead.signal
		}
	}

	def write1(v1: U) = monitor withLock{
		while(finishW1) canWrite.await		
		if(termination) { println("B stop"); stop}// check termination
		value1 = v1 
		finishW1 = true 
		println("B end write" + ' ' + value1)// Testing
		if(finishW0 && finishW1) { 
			readyToRead = true
			canRead.signal 
		}
	}

	def read: (T, U) = monitor withLock{
		while(!readyToRead) canRead.await 
		val v0 = value0; val v1 = value1 	
		finishW0 = false; finishW1 = false; readyToRead = false 
		println("Read values are: " + v0 + " "+ v1)// Testing

		//check for termination
		if(v0 == null.asInstanceOf[T] || v1 == null.asInstanceOf[T]){
			termination = true
			canWrite.signalAll
			println("read stop");
			stop
		}

		canWrite.signalAll		
		return (v0,v1)
	}
}

class SemaphoreZipSlotBounded[T,U] extends ZipSlot[T,U] {
	private val canWrite0 = new BooleanSemaphore(available = true)
	private val canWrite1 = new BooleanSemaphore(available = true)
	private val canRead = new CountingSemaphore(available = 0)

	//variables
	private var value0: T = _
	private var value1: U = _

	//flag
	private var termination = false

	//method
	def write0(v0: T) : Unit = {
		canWrite0.acquire	
		if(termination) { println("A stop"); stop}// check termination
		value0 = v0 
		println("A end write" + ' ' + value0)
		canRead.release
		
	}

	def write1(v1: U) : Unit = {
		canWrite1.acquire	
		if(termination) { println("B stop"); stop }// check termination
		value1 = v1 
		println("B end write" + ' ' + value1)
		canRead.release		
	}

	def read: (T, U) = {
		canRead.acquire
		canRead.acquire

		val v0 = value0; val v1 = value1
		println("The values are: " + v0 + " "+ v1) // Testing

		//check for termination
		if(v0 == null.asInstanceOf[T] || v1 == null.asInstanceOf[T]){
			termination = true
			canWrite0.release
			canWrite1.release
			println("read stop");
			stop
		}

		canWrite0.release
		canWrite1.release
		return (v0,v1)
	}
}

object Q1Termination{
	val start = nanoTime
	val random = new scala.util.Random
	val threshold = 0.05 // probobility of termination
	val myObject = new SemaphoreZipSlotBounded[Int, Int]
	//val myObject = new MonitorZipSlotBounded[Int, Int]

	def time[R](block: => R): R = {  
	    val t0 = nanoTime
	    val result = block    // call-by-name
	    val t1 = nanoTime
	    println("Elapsed time: " + (t1 - t0) + "ns")
	    result
	}

	def WriterA = proc("WriterA") {
		var done = false
		while (!done) {
			try{
				if(random.nextFloat > threshold){
					myObject.write0(random.nextInt(100))
				}else{
					myObject.write0(null.asInstanceOf[Int])
				}			
			}catch{
				case e: processimplementation.Stopped =>{
					//println("A termination")
					done = true							}	
			}
		}
	}

	def WriterB = proc("WriterB") {
		var done = false
		while (!done) {
			try{
				if(random.nextFloat > threshold){
					myObject.write1(random.nextInt(100))
				}else{
					myObject.write1(null.asInstanceOf[Int])
				}			
			}catch{
				case e: processimplementation.Stopped =>{
					done = true							}	
			}
		}

	}

	def Reader = proc("Reader") {
		var done = false
		while (!done) {
			try{
				val (v0,v1) = myObject.read		
			}catch{
				case e: processimplementation.Stopped =>{
					done = true							}	
			}
		}
  	}

  val System = WriterB || WriterA || Reader
  def main(args: Array[String]) = time{System()}
}