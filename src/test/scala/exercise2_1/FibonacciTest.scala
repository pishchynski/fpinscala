package exercise2_1

import org.scalatest.FunSuite

class FibonacciTest extends FunSuite {
  import Fibonacci._

  test("fib(1) should return 1") {
    assert(fib(1) === 1)
  }

  test("fib(5) should return 5") {
    assert(fib(5) === 5)
  }

  test("fib(7) should return 13") {
    assert(fib(7) === 13)
  }
}
