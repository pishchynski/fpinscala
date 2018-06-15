package exercise2_5

/**
  * Implement the higher-order function that composes two functions.
  */
object Composer {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}
