package exercise2_2

import org.scalatest.FunSpec

class SortedCheckTest extends FunSpec {
  import SortedCheck._

  describe("isSorted") {
    describe("For Array[Int]") {
      describe("With comparison function (a, b) => a < b") {

        def intCompFunc(a: Int, b: Int) = a < b

        describe("For Array {1, 2, 3}") {
          it("should return true") {
            val intArr = Array[Int](1, 2, 3)
            assert(isSorted[Int](intArr, intCompFunc))
          }
        }

        describe("For array {3, 1, 2}") {
          it("should return false") {
            val intArr = Array[Int](3, 1, 2)
            assert(!isSorted[Int](intArr, intCompFunc))
          }
        }
      }
    }
  }
}
