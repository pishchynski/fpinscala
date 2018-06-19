package exercises3_1

object MyListApp {

  sealed trait MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def sum(ints: MyList[Int]): Int = ints match {
      case MyNil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ints: MyList[Int]): Int = ints match {
      case MyNil => 1
      case Cons(0.0, _) => 0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): MyList[A] =
      if (as.isEmpty) MyNil
      else Cons(as.head, apply(as.tail: _*))

    // Ex3_2. Implement the function tail for removing the first element of a List. Note that the
    // function takes constant time.
    def tail[A](as: MyList[A]): MyList[A] = as match {
      case MyNil => MyNil
      case Cons(_, xs) => xs
    }

    // Ex3_3. Using the same idea, implement the function setHead for replacing the first element
    // of a MyList with a different value.
    def setHead[A](as: MyList[A], h: A): MyList[A] = {
      if (h == MyNil) MyNil
      else {
        as match {
          case MyNil => MyList(h)
          case Cons(_, xs) => Cons(h, xs)
        }
      }
    }

    // Ex3_4. Generalize tail to the function drop, which removes the first n elements from a list.
    def drop[A](as: MyList[A], n: Int): MyList[A] = as match {
      case MyNil => MyNil
      case Cons(_, xs) =>
        if (n == 1) xs
        else drop(xs, n - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    import MyList._

    // Ex3_1. What will be the result of the following match expression?
    val res = MyList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case MyNil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(res)

  }
}
