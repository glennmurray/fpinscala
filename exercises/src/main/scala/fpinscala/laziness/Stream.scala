package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    // The arrow `=>` in front of the argument type `B` means that the
    // function `f` takes its second argument by name and may choose not to
    // evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      // If `f` doesn't evaluate its second argument, the recursion never
      // occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  // Here `b` is the unevaluated recursive step that folds the tail of the
  // stream. If `p(a)` returns `true`, `b` will never be evaluated and the
  // computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1 Write a function to convert a Stream to a List, which will
  // force its evaluation and let you look at it in the REPL.
  // This done as toListRecursive and tail recursive in the answers.
  //def toList: List[A] = ???
  def toList: List[A] = this match {
    case Empty      => List.empty[A]
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2 Write the function take(n) for returning the first n
  // elements of a Stream, and drop(n) for skipping the first n elements of a
  // Stream.
  def take(n: Int): Stream[A] = ???

  def drop(n: Int): Stream[A] = ???

  // Exercise 5.3 Write the function takeWhile for returning all starting
  // elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}


///////////////////////////////////////////////////////////////////////////////

object StreamExercises {

    def main(args: Array[String]): Unit = {

      // Exercise 5.1
      //println(Stream(1, 2, 3))  // Prints a mess.
      //println(Stream(1, 2, 3).toList) // List(1, 2, 3)
      assert(Stream(1, 2, 3).toList equals List(1, 2, 3))



    }
}
