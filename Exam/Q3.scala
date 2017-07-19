import io.threadcso._ 
import io.threadcso.semaphore.Flag
import scala.collection.mutable.ListBuffer

abstract class Classroom(s: Int, p: Int, t: Int){
	def S(id: Int): List[Int]
	def P(id: Int): List[Int]
	def T(id: Int): List[Int]
	def Leave(id: Int): Unit
}

class MyClassroom(s: Int, p: Int, t: Int) extends Classroom(s, p, t){
	//queue and semaphore
	val queueWaiting = new scala.collection.mutable.Queue[Flag]
	val mutexEnter = BooleanSemaphore(true)
	val mutexLeave = BooleanSemaphore(true)
	val minSize = s+p+t
	val myList = new ListBuffer[Int] //quorum

	//counter
	var numStudent = 0
	var numTutor = 0
	var numProfessor = 0
	var classSize = 0

	//function
	def Arrive: List[Int] = {
		if(numStudent >= s && numProfessor >= p && numTutor >= t){
			println();println()
			println("*** Class Begin ***")
			print("Participants: ")
			print(numStudent + " Sdudents, ")
			print(numProfessor + " Professors, ")
			println(numTutor + " Tutors, ")
			print("IDs : ")
			println(myList mkString  " "); 
			println()

			//a quorum formed
			classSize = numStudent + numProfessor + numTutor		
			//dequeue all
			for(i <- 0 until queueWaiting.length){ 
				val flag = queueWaiting.dequeue
				flag.release
			}
			myList.toList //return the list

		}else{
			//wait for the quorum to form
			val flag = new Flag
			queueWaiting.enqueue(flag)
			mutexEnter.release
			flag.acquire
			myList.toList //return the list
		}
	}

	def S(id: Int): List[Int] = {
		mutexEnter.acquire
		numStudent+=1 // increase the counter
		myList += id // store id
		print("students" + ' '+ id + ' ' + "enter, ")
		Arrive	
	}

	def P(id: Int): List[Int] = {
		mutexEnter.acquire
		numProfessor+=1 // increase the counter
		myList += id // store id
		print("Professor" + ' '+ id + ' ' + "enter, ")
		Arrive		
	}

	def T(id: Int): List[Int] = {
		mutexEnter.acquire
		numTutor+=1 // increase the counter
		myList += id // store id
		print("Tutor" + ' '+ id + ' ' + "enter, ")
		Arrive
	}

	def Leave(id: Int): Unit = {
		mutexLeave.acquire
		classSize -= 1
		print(id + " leave, ")

		if(classSize == 0){
			println()
			println("*** Class End ***")
			println()
			//reset
			numStudent = 0; numProfessor = 0; numTutor = 0; myList.clear
			mutexEnter.release
		}
		mutexLeave.release		
	}
}

object Q3{
	val s = 2;val p = 2;val t = 2;
	print("The quorum setting is: ")
	println(s + " Students, " + p + " Professors, " + t + " Tutors ")
	print("------------------------------------------------------------")
	println()
	val myObject = new MyClassroom(s,p,t)
	val random = new scala.util.Random
	val threshold = 10

	val Students = ||(for (i<-0 until threshold) yield
		proc{
			repeat {
				val classList = myObject.S(i);
				sleep(random.nextInt(6)*Sec) 
				myObject.Leave(i)
				sleep(random.nextInt(6)*Sec)
			}
		})

	val Tutors = ||(for (j<-threshold until threshold*2) yield
		proc{ 
			repeat {
				val classList = myObject.T(j);
				sleep(random.nextInt(6)*Sec) 
				myObject.Leave(j)
				sleep(random.nextInt(6)*Sec)
			}
		})

	val Professors = ||(for (k<-threshold*2 until threshold*3) yield
		proc{ 
			repeat {
				val classList = myObject.P(k); 
				sleep(random.nextInt(6)*Sec) 
				myObject.Leave(k) 
				sleep(random.nextInt(6)*Sec)
			}
		})

	val System = Students || Tutors || Professors
	def main(args: Array[String]) = System()
}
