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

    describe("when drop invoked") {
      describe("for empty MyList") {
        describe("and 1 as a number of elements to drop") {
          it("should return MyNil") {
            assert(MyList.drop(MyList(), 1) === MyNil)
          }
        }
      }

      describe("for MyList(1, 2, 3)") {
        describe("and 1 as a number of elements to drop") {
          it("should return MyList(2, 3)") {
            assert(MyList.drop(MyList(1, 2, 3), 1) === MyList(2, 3))
          }
        }

        describe("and 3 as a number of elements to drop") {
          it("should return MyNil") {
            assert(MyList.drop(MyList(1, 2, 3), 3) === MyNil)
          }
        }

        describe("and 10 as a number of elements to drop") {
          it("should return MyNil") {
            assert(MyList.drop(MyList(1, 2, 3), 10) === MyNil)
          }
        }
      }
    }

    describe("when dropWhile invoked") {
      describe("for empty MyList") {
        describe("and x => x == MyNil as a predicate") {
          it("should return MyNil") {
            assert(MyList.dropWhile[Any](MyList())(x => x == MyNil) === MyNil)
          }
        }
      }

      describe("for MyList(1, 2, 3)") {
        describe("and x => x < 3 as a predicate") {
          it("should return MyList(3)") {
            assert(MyList.dropWhile(MyList(1, 2, 3))(x => x < 3) === MyList(3))
          }
        }

        describe("and x => x < 5 as a predicate") {
          it("should return MyNil)") {
            assert(MyList.dropWhile(MyList(1, 2, 3))(x => x < 5) === MyNil)
          }
        }
      }
    }

    describe("when init invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.init(MyList()) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return MyList(3)") {
          assert(MyList.init(MyList(1, 2, 3)) === MyList(1, 2))
        }
      }

      describe("for MyList(1)") {
        it("should return MyNil") {
          assert(MyList.init(MyList(1)) === MyNil)
        }
      }
    }

    describe("when foldRight invoked") {
      describe("for MyList(1, 2, 3)") {
        describe("and MyNil as a first element") {
          describe("and Cons(_, _) as a function to be applied") {
            it("should return MyList(1, 2, 3)") {
              assert(MyList.foldRight(MyList(1,2,3), MyNil: MyList[Int])(Cons(_,_)) === MyList(1, 2, 3))
            }
          }
        }
      }
    }

    describe("when length invoked") {
      describe("for empty MyList") {
        it("should return 0") {
          assert(MyList.length(MyList()) === 0)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return 3") {
          assert(MyList.length(MyList(1, 2, 3)) === 3)
        }
      }

      describe("for MyList(1)") {
        it("should return 1") {
          assert(MyList.length(MyList(1)) === 1)
        }
      }
    }

    describe("when sum3 invoked") {
      describe("for empty MyList") {
        it("should return 0") {
          assert(MyList.sum3(MyList()) === 0)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return 6") {
          assert(MyList.sum3(MyList(1, 2, 3)) === 6)
        }
      }
    }

    describe("when product3 invoked") {
      describe("for empty MyList") {
        it("should return 0") {
          assert(MyList.product3(MyList()) === 1)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return 6") {
          assert(MyList.product3(MyList(1, 2, 3)) === 6)
        }
      }
    }
  }
}
