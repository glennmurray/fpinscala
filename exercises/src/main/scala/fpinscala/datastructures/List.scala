package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  //def tail[A](l: List[A]): List[A] = ???
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  //def setHead[A](l: List[A], h: A): List[A] = ???
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, List.tail(l))

  //def drop[A](l: List[A], n: Int): List[A] = ???
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else {
      l match {
        case Nil => Nil
        case _ => List.drop(List.tail(l), n - 1)
      }
    }
  }

  //def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???
//  @annotation.tailrec
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//    case Nil => Nil
//    case Cons(h, t) =>
//      if (f(h)) dropWhile(t, f)
//      else l
//  }
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6 Returns a List consisting of all but the last element of a List.
  //def init[A](l: List[A]): List[A] = ???
  // NOT @annotation.tailrec
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // Exercise 3.9.
  //def length[A](l: List[A]): Int = ???
  // NOT @annotation.tailrec
//  def length[A](l: List[A]): Int = l match {
//    case Nil => 0
//    case Cons(h, t) => 1 + length(t)
//  }
  // Use foldRight.
  def length[A](l: List[A]): Int = {
    List.foldRight(l, 0)((_, i) => i + 1)
  }

  // Exercise 3.10.  Write a tail-recursive foldLeft.
  //def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???
  // Mine: def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  //  case Nil => z
  //  case Cons(h, t) => f( foldLeft(t, z)(f),  h )
  //}
  // From the answers.
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // Ex. 3.11 Write sum, product, and length with foldLeft.
  def sumFoldLeft(l: List[Int]): Int = List.foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Int]): Int = List.foldLeft(l, 1)(_ * _)
  def lengthFoldLeft(l: List[Int]): Int = List.foldLeft(l, 0)((i, _) => i + 1)

  // Ex. 3.12 Write reverse.
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((revList, y) => Cons(y, revList))
  }

  // Ex. 3.13 (Hard) Write foldLeft in terms of foldRight.

  // Ex. 3.14 Implement append with foldLeft or Right.
  def appendFold[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l2, l1)((as, a) => Cons(a, as))
  }

  // Exercise 3.18
  //def map[A,B](l: List[A])(f: A => B): List[B] = ???
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }
}

object ListExercises {
  // Exercise 3.1
  val x = fpinscala.datastructures.List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  def main(args: Array[String]): Unit = {
    // Exercise 3.1
    assert(x == 3)

    val l = fpinscala.datastructures.List(1, 2, 3)

    // Ex. 3.2, implement tail.
    assert(List.tail(l) match {
      case Cons(2, Cons(3, Nil)) => true
      case _ => false
    })

    // Ex. 3.3, implement setHead.
    assert(List.setHead(l, 4) match {
      case Cons(4, Cons(2, Cons(3, Nil))) => true
      case _ => false
    })

    // Ex. 3.4 implement drop.
    assert(List.drop(l, 2) match {
      case Cons(3, Nil) => true
      case _ => false
    })

    // Ex. 3.5 implement dropWhile.
    def lte2(i: Int): Boolean = i <= 2
    assert(List.dropWhile(l, lte2) match {
      case Cons(3, Nil) => true
      case _ => false
    })

    // Ex. 3.6 implement init.
    assert(List.init(l) match {
      case Cons(1, (Cons(2, Nil))) => true
      case _ => false
    })
    assert(List.init(List.tail(l)) match {
      case Cons(2, Nil) => true
      case _ => false
    })

    // Exercise 3.9.  Implement length.
    assert(List.length(l) match {
      case 3 => true
      case _ => false
    })

    // Exercise 3.10.  Implement foldLeft
    //import scala.collection.immutable
    //val answer = immutable.List(2, 3).foldLeft(0)(_ - _)
    //println(immutable.List(2, 3).foldRight(0)(_ - _))
    def scale(i: Int, j: Int): Int = i - j
    assert(List.foldLeft( fpinscala.datastructures.List(2, 3), 0)(scale) match {
      case -5 => true
      case e => println(e); false
    })

    // Exercise 3.11.  Sum, product, length with foldLeft.
    assert((List.sumFoldLeft(l), List.productFoldLeft(l), List.lengthFoldLeft(l)) match {
      case (6, 6, 3) => true
      case _ => false
    })
    // For lengthFoldLeft: Cons(h,t) => foldLeft(t, f(z,h))(f)
    //    List.foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((i, _) => i + 1)
    //            List.foldLeft(Cons(2, Cons(3, Nil)),  1)((i, _) => i + 1)
    //                    List.foldLeft(Cons(3, Nil),   2)((i, _) => i + 1)
    //                            List.foldLeft(Nil,    3)((i, _) => i + 1)

    // Ex. 3.12 Write reverse.
    assert(List.reverse(l) match {
      case Cons(3, Cons(2, Cons(1, Nil))) => true
      case _ => false
    })

    // Ex. 3.14 Implement append with foldLeft or Right.
    assert(List.appendFold(l, Cons(4, Nil)) match {
      case Cons(4, Cons(3, Cons(2, Cons(1, Nil)))) => true
      case _ => false
    })


    // Exercise 3.18.  Implement map.
    assert(List.map(l)(i => 2 * i) match {
      case Cons(2, Cons(4, Cons(6, Nil))) => true
      case _ => false
    })
  }

}