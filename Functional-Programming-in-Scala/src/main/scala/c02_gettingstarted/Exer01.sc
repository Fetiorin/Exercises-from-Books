def fib(n: Int): Int = {
  def loop(n: Int, prev: Int, curr: Int): Int = {
    if (n == 0) prev
    else loop(n-1, curr, curr + prev)
  }
  loop(n, 0, 1)
}

(0 to 10).toList.map(fib)
