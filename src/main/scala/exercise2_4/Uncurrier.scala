package exercise2_4

/**
  * Implement uncurry, which reverses the transformation of curry.
  */
object Uncurrier {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}
