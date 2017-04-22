def sign(num: Int): Int = num match {
  case n if n > 0 => 1
  case n if n < 0 => -1
  case _ => 0
}

def countdown(n: Int) = {
  for (i <- n to 0 by -1) print(i + " ")
}

def productRec(s: String, acc: Long = 1): Long = {
  if (s.length == 1)
    s.head*acc
  else
    productRec(s.tail, acc*s.head)
} 

def powto(x: Double, num: Long):Double = num match {
  case n if (n % 2 == 0 && n > 0)  => 
    val y = powto(x, n / 2)
    y*y
  case n if n > 0 =>
    x*powto(x, n-1)
  case 0 => 1
  case n =>
    1.0/powto(x, -n)
} 
