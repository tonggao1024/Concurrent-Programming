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

class MonitorZipSlot[T,U] extends ZipSlot[T,U] {
	private val monitor = new Monitor
	private val canRead = monitor.newCondition 
	private val canWrite = monitor.newCondition

	//flag
	private var finishW0 = false
	private var finishW1 = false
	private var readyToRead = false

	//variables
	private var value0: T = _
	private var value1: U = _

	//method
	def write0(v0: T) = monitor withLock{
		while(finishW0) canWrite.await // wait until flag is reset

		value0 = v0 // write to value0
		finishW0 = true // set flag
		println("A end write" + ' ' + value0) // testing
		// signal read if both write finish
		if(finishW0 && finishW1) { 
			readyToRead = true
			canRead.signal 
		} 
	}

	def write1(v1: U) = monitor withLock{
		while(finishW1) canWrite.await // wait until flag is reset

		value1 = v1 // write to value1
		finishW1 = true // set flag
		println("B end write" + ' ' + value1)// testing
		// signal read if both write finish
		if(finishW0 && finishW1) { 
			readyToRead = true
			canRead.signal 
		} 
	}

	def read: (T, U) = monitor withLock{
		while(!readyToRead) canRead.await // wait until both writes done

		// read values
		val v0 = value0; val v1 = value1 
		// reset flags	
		finishW0 = false; finishW1 = false; readyToRead = false 
		println("Read values are: " + v0 + " "+ v1)// testing
		canWrite.signalAll //signal both writes
		
		return (v0,v1)
	}
}

class SemaphoreZipSlot[T,U] extends ZipSlot[T,U] {
	private val canWrite0 = new BooleanSemaphore(available = true)
	private val canWrite1 = new BooleanSemaphore(available = true)
	private val canRead = new CountingSemaphore(available = 0)

	//variables
	private var value0: T = _
	private var value1: U = _

	//method
	def write0(v0: T) : Unit = {
		canWrite0.acquire
		value0 = v0 //write to value0
		println("A end write" + ' ' + value0)
		canRead.release
	}

	def write1(v1: U) : Unit = {
		canWrite1.acquire
		value1 = v1 //write to value1
		println("B end write" + ' ' + value1)
		canRead.release
	}

	def read: (T, U) = {
		canRead.acquire
		canRead.acquire

		val v0 = value0; val v1 = value1 // read values	

		println("Read values are: " + v0 + " "+ v1)

		//release writes
		canWrite0.release
		canWrite1.release

		return (v0,v1)
	}
}

object Q1{
	val start = nanoTime
	val random = new scala.util.Random
	val r = scala.util.Random
	val myObject = new MonitorZipSlot[Int, Int] // class instance
	//val myObject = new SemaphoreZipSlot[Int, Int] // class instance

	def WriterA = proc("WriterA") {
		while (true) {
			sleep(random.nextInt(3)*Sec)
			myObject.write0(r.nextInt(100))		
		}
	}

	def WriterB = proc("WriterB") {
		while (true) {
			sleep(random.nextInt(3)*Sec)
			myObject.write1(r.nextInt(100))			
		}
	}

	def Reader = proc("Reader") {
	    while (true) { 
	    	val (v0,v1) = myObject.read	
	    }
  	}

  val System = WriterB || WriterA || Reader

  def main(args: Array[String]) = System()
}