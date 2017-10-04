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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
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


  // Ex. 3.13 (Hard) Write foldLeft in terms of foldRight, and vice-versa.
  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    //foldLeft(reverse(as), z)((b, a) => f(a, b))  // or
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }


  // Ex. 3.14 Implement append with foldLeft or Right.
  def appendFoldR[A](l1: List[A], l2: List[A]): List[A] = {
    //List.foldRight(l1, l2)((a, l2s) => Cons(a, l2s))
    List.foldRight(l1, l2)(Cons(_, _))
  }

  // Ex. 3.15 Concatenate a list of lists into a single list.
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  // Ex. 3.16 Write a function that increments a List[Int].
  def add1(l: List[Int]): List[Int] = {
    //reverse(foldLeft(l, Nil: List[Int])((x, y) => Cons(y + 1, x )))
    foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  //Ex. 3.17 Write a function that turns each value in a List[Double]
  // into a String.
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  // Exercise 3.18 Implement map.  Here are two solutions.
  //def map[A,B](l: List[A])(f: A => B): List[B] = ???
  // def map[A,B](l: List[A])(f: A => B): List[B] = l match {
  //   case Nil => Nil
  //   case Cons(h, t) => Cons(f(h), map(t)(f))
  // }
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  // Ex. 3.19 Write a function filter that removes elements from a list
  // unless they satisfy a given predicate.
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((x, y) => if (f(x)) Cons(x,y) else y)
  }

  // Ex. 3.20 Write a function flatMap that works like map except that
  // the function given will return a list instead of a single result,
  // and that list should be inserted into the final resulting list.
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    //concat(map(l)(f))   (answer) or mine:
    foldRight(l, Nil: List[B])((x, y) => append(f(x), y))
  }

  // Ex. 3.21  Use flatMap to implement filter.
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  // Ex. 3.22 Write a function that accepts two lists and constructs a
  // new list by adding corresponding elements. For example, List(1,2,3)
  // and List(4,5,6) become List(5,7,9).
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, has), Cons(hb, hbs)) =>
      Cons(ha + hb, addPairwise(has, hbs))
  }

  // Ex. 3.23 Generalize the function you just wrote so that it’s not
  // specific to integers or addition. Name it zipWith.
  def zipWith[A,B,C](a: List[A], b: List[B])(f:(A, B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, has), Cons(hb, hbs)) =>
      Cons(f(ha, hb), zipWith(has, hbs)(f))
  }

  // Ex. 3.24 Hard: As an example, implement hasSubsequence for checking
  // whether a List contains another List as a subsequence.
  def hasSubsequence[A](sup: List[A], sub: List[A])
      : Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(ha, has), Cons(hb, hbs)) => hasSubsequence(has, hbs)
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
    def subtr(i: Int, j: Int): Int = i - j
    assert(List.foldLeft(List(2, 3), 0)(subtr) match {
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

    // Ex. 3.13 Implement foldRight with  foldLeft.
    val z = 0; def scale(i: Int, j: Int) = 2 * i + j
    assert(List.foldRightViaFoldLeft(l, z)(scale) match {
      case 12 => true
      case _ =>
        val expected = List.foldRight(l, z)(scale)
        println(s"\nEx. 3.13 foldRightViaFoldLeft\nexpected: $expected")
        val result = List.foldRightViaFoldLeft(l, z)(scale)
        println(  s"result  : $result")
        false
    })
    // foldRight(l, 0)((x, y) => 2 * x + y
    // f(1, foldRight(L(2,3), 0))
    // f(1, f(2, foldRight(L(3), 0)))
    // f(1, f(2, f(3, 0)))
    // f(1, f(2, 6))
    // f(1, 10) = 12



    // Ex. 3.14 Implement append with foldLeft or Right.
    assert(List.appendFoldR(l, Cons(4, Cons(5, Nil))) match {
      case Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))) => true
      case _ =>
        val expected = List.append(l, Cons(4, Cons(5, Nil)))
        println(s"Ex. 3.14\nexpected: $expected")
        val result = List.appendFoldR(l, Cons(4, Cons(5, Nil)))
        println(  s"result  : $result")
        false
    })

    // Ex. 3.15 Concatenate a list of lists into a single list.
    assert(List.concat(List(l, l)) match {
      case Cons(1, Cons(2, Cons(3, Cons(1, Cons(2, Cons(3, Nil)))))) => true
      case _ => false
    })

    // Ex. 3.16 Write a function that increments a List[Int].
    assert(List.add1(l) match {
      case Cons(2, Cons(3, Cons(4, Nil))) => true
      case _ => false
    })

    //Ex. 3.17 Write a function that turns each value in a List[Double]
    // into a String.
    assert(List.doubleToString(Cons(3.0, (Cons(4.0, Nil)))) match {
      case Cons("3.0", (Cons("4.0", Nil))) => true
      case _ => false
    })

    // Ex. 3.19 Write a function filter that removes elements from a list
    // unless they satisfy a given predicate.
    assert(List.filter(List.concat(List(l, l)))(_ % 2 == 0) match {
      case Cons(2, Cons(2, Nil)) => true
      case _ => false
    })

    // Exercise 3.18.  Implement map.
    assert(List.map(l)(i => 2 * i) match {
      case Cons(2, Cons(4, Cons(6, Nil))) => true
      case _ => false
    })

    // Ex. 3.20 Implement flatMap.
    assert(List.flatMap(l)(i => List(i,i)) match {
      case Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil)))))) => true
      case _ => false
    })

    // Ex. 3.21  Use flatMap to implement filter.
    assert(List.filterViaFlatMap(List.concat(List(l, l)))(_ % 2 == 0) match {
      case Cons(2, Cons(2, Nil)) => true
      case _ => false
    })

    // Ex. 3.22 Write a function that accepts two lists and constructs a
    // new list by adding corresponding elements. For example, List(1,2,3)
    // and List(4,5,6) become List(5,7,9).
    assert(List.addPairwise(l, l) match {
      case Cons(2, Cons(4, Cons(6, Nil))) => true
      case _ => false
    })


    // Ex. 3.23 Generalize the function you just wrote so that it’s not
    // specific to integers or addition. Name it zipWith.
    assert(List.zipWith(l, l)((x, y) => x * y) match {
      case Cons(1, Cons(4, Cons(9, Nil))) => true
      case _ => false
    })

    // Ex. 3.24 Hard: As an example, implement hasSubsequence for checking
    // whether a List contains another List as a subsequence.
    assert(List.hasSubsequence(l, l))
    assert(List.hasSubsequence(l, Cons(3, Nil)))
    assert(List.hasSubsequence(l, Cons(1, Nil)))
    assert(List.hasSubsequence(l, Cons(1, Cons(2, Nil))))
    assert(List.hasSubsequence(l, Cons(2, Cons(3, Nil))))

  }
}
