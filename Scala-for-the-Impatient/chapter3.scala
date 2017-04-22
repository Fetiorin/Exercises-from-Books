import util.Random
import scala.collection.mutable.ArrayBuffer

def fun1(n: Int): Array[Int] = {
  val a = new Array[Int](n)
  for (i <- 0 until n) a(i) = Random.nextInt(n)
  a
}

def fun2(a: Array[Int]): Array[Int] = {
	//val a = Array( ... )
	for (i <- 1 until a.length by 2) {
		val t = a(i-1)
		a(i-1) = a(i)
		a(i) = t
	}
	a
}

def fun3(a: Array[Int]): Array[Int] = {
  (for (i  <- 0 until a.length) yield {
    if (i % 2 != 0) a(i-1)
    else if (i+1 == a.length) a(i) 
    else  a(i+1)
  }).toArray
}

def fun4(a: Array[Int]): Array[Int] = {
  val parts = a.partition(_ < 0)
  parts._1 ++ parts._2
}

def fun5(a: Array[Double]): Double = {
  a.sum / a.length
}

def fun6(a: Array[Int], b: ArrayBuffer[Int]): Unit = {
  println(a.sortWith(_ > _) mkString " ")
  println(b.sortWith(_ > _) mkString " ")
}

def fun7(a: Array[Int]): Array[Int] = {
  a.distinct
}

def fun8(a: ArrayBuffer[Int]): ArrayBuffer[Int] = {
  val indexes = (for (i <- a.indices if a(i) <= 0) yield i)
    .reverse.dropRight(1)
  for (i <- indexes) a.remove(i)
  a
}