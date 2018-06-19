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

    describe("when setHead invoked") {
      describe("for empty MyList") {
        describe("and 1 as a new head") {
          it("should return MyList(1)") {
            assert(MyList.setHead(MyList(), 1) === MyList(1))
          }
        }

        describe("and MyNil as a new head") {
          it("should return MyNil") {
            assert(MyList.setHead(MyList(), MyNil) === MyNil)
          }
        }
      }

      describe("for MyNil") {
        describe("""and "one" as a new head""") {
          it("""should return MyList("one")""") {
            assert(MyList.setHead(MyNil, "one") === MyList("one"))
          }
        }

        describe("and MyNil as a new head") {
          it("should return MyNil") {
            assert(MyList.setHead(MyNil, MyNil) === MyNil)
          }
        }
      }

      describe("for MyList(1, 2, 3)") {
        describe("and MyNil as a new head") {
          it("should return MyNil") {
            assert(MyList.setHead(MyList(1, 2, 3), MyNil) === MyNil)
          }
        }

        describe("and 10 as a new head") {
          it("should return MyList(10, 2, 3)") {
            assert(MyList.setHead(MyList(1, 2, 3), 10) === MyList(10, 2, 3))
          }
        }
      }

      describe("for MyList(1)") {
        describe("and MyNil as a new head") {
          it("should return MyNil") {
            assert(MyList.setHead(MyList(1), MyNil) === MyNil)
          }
        }

        describe("and 10 as a new head") {
          it("should return MyList(10)") {
            assert(MyList.setHead(MyList(1), 10) === MyList(10))
          }
        }
      }

      describe("""for MyList("one", "two", "three")""") {
        describe("""and "ten" as a new head""") {
          it("""should return MyList("ten", "two", "three")""") {
            assert(MyList.setHead(MyList("one", "two", "three"), "ten") === MyList("ten", "two", "three"))
          }
        }
      }
    }
  }
}
