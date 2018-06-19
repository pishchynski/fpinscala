package exercises3_1

import org.scalatest.FunSpec

class MyListTest extends FunSpec {
  import MyListApp._

  describe("MyList") {
    describe("when tail invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.tail(MyList()) === MyNil)
        }
      }

      describe("for MyNil") {
        it("should return MyNil") {
          assert(MyList.tail(MyNil) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return MyList(2, 3)") {
          assert(MyList.tail(MyList(1, 2, 3)) === MyList(2, 3))
        }
      }

      describe("for MyList(1)") {
        it("should return MyNil") {
          assert(MyList.tail(MyList(1)) === MyNil)
        }
      }

      describe("""for MyList("one", "two", "three")""") {
        it("""should return MyList("two", "three")""") {
          assert(MyList.tail(MyList("one", "two", "three")) === MyList("two", "three"))
        }
      }
    }
  }
}
