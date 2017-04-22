def fib(n: Int): Int = {
  def loop(prev: Int, curr: Int, i: Int): Int = {
    if (i == n) curr
    else loop(curr, curr+prev, i + 1)
  }
  if (n == 1) 0
  else loop(0, 1, 2)
}

(1 to 10).toList.map(fib)

