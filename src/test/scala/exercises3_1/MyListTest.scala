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

    describe("when length3 invoked") {
      describe("for empty MyList") {
        it("should return 0") {
          assert(MyList.length3(MyList()) === 0)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return 3") {
          assert(MyList.length3(MyList(1, 2, 3)) === 3)
        }
      }

      describe("for MyList(1)") {
        it("should return 1") {
          assert(MyList.length3(MyList(1)) === 1)
        }
      }
    }

    describe("when reversed invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.reversed(MyList()) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return MyList(3, 2, 1)") {
          assert(MyList.reversed(MyList(1, 2, 3)) === MyList(3, 2, 1))
        }
      }
    }

    describe("when reversed2 invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.reversed2(MyList()) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return MyList(3, 2, 1)") {
          assert(MyList.reversed2(MyList(1, 2, 3)) === MyList(3, 2, 1))
        }
      }
    }

    describe("when foldRightViaLeft invoked") {
      describe("for MyList(1, 2, 3)") {
        describe("and MyNil as a first element") {
          describe("and Cons(_, _) as a function to be applied") {
            it("should return MyList(1, 2, 3)") {
              assert(MyList.foldRightViaLeft(MyList(1,2,3), MyNil: MyList[Int])(Cons(_,_)) === MyList(1, 2, 3))
            }
          }
        }
      }
    }

    describe("when append invoked") {
      describe("for empty MyList") {
        describe("and MyList(1) to append") {
          it("should return MyList(1)") {
            assert(MyList.append(MyList(), MyList(1)) === MyList(1))
          }
        }

        describe("""and "one" to append""") {
          describe("""and MyList("one") to append""") {
            it("""should return MyList("one")""") {
              assert(MyList.append(MyList(), MyList("one")) === MyList("one"))
            }
          }
        }
      }

      describe("for MyList(1, 2, 3)") {
        describe("and MyList(9, 10) to append") {
          it("should return MyList(1, 2, 3, 9, 10)") {
            assert(MyList.append(MyList(1, 2, 3), MyList(9, 10)) === MyList(1, 2, 3, 9, 10))
          }
        }
      }
    }

    describe("when flatten invoked") {
      describe("for two empty lists") {
        it("should return MyNil") {
          assert(MyList.append(MyList(), MyList()) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3) and MyList(4, 5, 6)") {
        it("should return MyList(1, 2, 3, 4, 5, 6)") {
          assert(MyList.append(MyList(1, 2, 3), MyList(4, 5, 6)) === MyList(1, 2, 3, 4, 5, 6))
        }
      }
    }

    describe("when addOne invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.addOne(MyList()) === MyNil)
        }
      }

      describe("for MyList(1, 2, 3)") {
        it("should return MyList(2, 3, 4)") {
          assert(MyList.addOne(MyList(1, 2, 3)) === MyList(2, 3, 4))
        }
      }
    }

    describe("when fromDoubleToString invoked") {
      describe("for empty MyList") {
        it("should return MyNil") {
          assert(MyList.fromDoubleToString(MyList[Double]()) === MyNil)
        }
      }

      describe("for MyList(1.0, 2.22, 3.14)") {
        it("""should return MyList("1.0", "2.22", "3.14")""") {
          assert(MyList.fromDoubleToString(MyList(1.0, 2.22, 3.14)) === MyList("1.0", "2.22", "3.14"))
        }
      }
    }

    describe("when map invoked") {
      describe("for empty MyList") {
        describe("and function x => x + 1") {
          it("should return MyNil") {
            assert(MyList.map[Int, Int](MyList())(x => x + 1) === MyNil)
          }
        }
      }

      describe("for MyList(1, 2, 3)") {
        describe("and function x => x * x") {
          it("should return MyList(1, 4, 9)") {
            assert(MyList.map(MyList(1, 2, 3))(x => x * x) === MyList(1, 4, 9))
          }
        }
      }
    }
  }
}
