package exercise2_1

/**
  * Write a recursive function to get the nth Fibonacci number.
  * The first two Fibonacci numbers are 0 and 1.
  * Your definition should use a local tail-recursive function.
  */
object Fibonacci {
  /**
    * A function to get the nth Fibonacci number.
    * The first two Fibonacci numbers are 0 and 1.
    *
    * @param n The index of Fibonacci number to be calculated
    * @return nth Fibonacci number
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(prevPrev: Int, prev: Int, n: Int): Int = {
      if (n <= 1) prev
      else go(prev, prev + prevPrev, n - 1)
    }

    go(0, 1, n)
  }
}
