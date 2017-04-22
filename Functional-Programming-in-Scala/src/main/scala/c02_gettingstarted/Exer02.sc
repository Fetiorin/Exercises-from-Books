def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  def loop(curr: Int): Boolean = {
    if (curr >= as.length) true
    else if (gt(as(curr), as(curr - 1))) loop(curr + 1)
    else false
  }
  loop(1)
}

isSorted(Array(1,1,2,3,4,5), (a: Int, b: Int) => a >= b)
