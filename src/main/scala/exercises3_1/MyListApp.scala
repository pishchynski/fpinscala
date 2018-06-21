package exercises3_1

import scala.annotation.tailrec

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
    def tail[A](l: MyList[A]): MyList[A] = l match {
      case MyNil => MyNil
      case Cons(_, xs) => xs
    }

    // Ex3_3. Using the same idea, implement the function setHead for replacing the first element
    // of a MyList with a different value.
    def setHead[A](l: MyList[A], h: A): MyList[A] = {
      if (h == MyNil) MyNil
      else {
        l match {
          case MyNil => MyList(h)
          case Cons(_, xs) => Cons(h, xs)
        }
      }
    }

    // Ex3_4. Generalize tail to the function drop, which removes the first n elements from a list.
    def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
      case MyNil => MyNil
      case Cons(_, xs) =>
        if (n == 1) xs
        else drop(xs, n - 1)
    }

    // Ex3_5. Implement dropWhile, which removes elements from the List prefix as long as they
    // match a predicate.
    def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
      case MyNil => MyNil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs)(f)
        else l
    }

    // Ex3_6. Implement a function, init, that returns a List consisting of all
    // but the last element of a List.
    def init[A](l: MyList[A]): MyList[A] = l match {
      case MyNil => MyNil
      case Cons(_, MyNil) => MyNil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    // Lst3_2. Right folds and simple uses
    def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
        case MyNil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: MyList[Int]): Int =
      foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: MyList[Double]): Double =
      foldRight(ns, 1.0)(_ * _)

    // Ex3_9. Compute the length of a list using foldRight.
    def length[A](as: MyList[A]): Int = {
      foldRight(as, 0)((_, y) => y + 1)
    }

    @tailrec
    def foldLeft[A,B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
      case MyNil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum3(ns: MyList[Int]): Int =
      foldLeft(ns, 0)((x,y) => x + y)

    def product3(ns: MyList[Double]): Double =
      foldLeft(ns, 1.0)(_ * _)

    def length3[A](ns: MyList[A]): Int = {
      foldLeft(ns, 0)((x, _) => x + 1)
    }

    // Ex3_12. Write a function that returns the reverse of a list.
    def reversed[A](ns: MyList[A]): MyList[A] = {
      @tailrec
      def go(ol: MyList[A], nl: MyList[A]) : MyList[A] = ol match {
        case MyNil => nl
        case Cons(x, xs) => go(xs, Cons(x, nl))
      }

      go(ns, MyList())
    }

    def reversed2[A](ns: MyList[A]): MyList[A] = {
      foldLeft(ns, MyList[A]())((x, y) => Cons(y, x))
    }

    // Ex3_13. Can you write foldRight in terms of foldLeft?
    def foldRightViaLeft[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reversed2(as), z)((x, y) => f(y, x))
    }

    // Ex3_14. Implement append in terms of either foldLeft or foldRight.
    def append[A](l: MyList[A], r: MyList[A]): MyList[A] = {
      foldRight(l, r)(Cons(_, _))
    }

    // Ex3_15. Write a function that concatenates a list of lists into a single list. Its runtime
    // should be linear in the total length of all lists. Try to use functions we have already
    // defined.
//    def flatten[A](ls: MyList[MyList[A]]): MyList[A] ={
//
//    }
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
