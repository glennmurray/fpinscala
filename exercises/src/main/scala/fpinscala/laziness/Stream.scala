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
  //def take(n: Int): Stream[A] = ???
  //@annotation.tailrec
  def take(n: Int): Stream[A] = this match {
    case Empty                => Empty
    case Cons(h, t) if n < 1  => this
    case Cons(h, t) if n == 1 => cons( h(), Empty )
    //case Cons(h, t) if n > 0 => Cons( () => h(), () => t().take(n - 1) )
    case Cons(h, t)           => cons( h(), t().take(n - 1) )

  }
  //def drop(n: Int): Stream[A] = ???
  def drop(n: Int): Stream[A] = this match {
    case Empty                => Empty
    case Cons(h, t) if n == 0 => this
    case Cons(h, t)           => t().drop(n - 1)
  }

  // Exercise 5.3 Write the function takeWhile for returning all starting
  // elements of a Stream that match the given predicate.
  //def takeWhile(p: A => Boolean): Stream[A] = ???
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => Empty
  }

  // Exercise 5.4  Implement forAll, which checks that all elements in the
  // Stream match a given predicate. Your implementation should terminate the
  // traversal as soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  //Exercise 5.5  Use foldRight to implement takeWhile.
  def takeWhileFr(p: A => Boolean): Stream[A] =
    foldRight(empty[A])( (h, t) => if (p(h)) cons(h, t) else Empty )

  def takeWhileFrAnswer(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else      empty)

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


/**  The companion object.  */
object Stream {
  /**
    * The "smart" constructor.
    */
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
      assert(Stream(1, 2, 3).toList == List(1, 2, 3))

      // Exercise 5.2
      assert( Empty.take(1) == Empty )
      val s52 = Stream(1, 2, 3, 4)
      //println(Stream(1, 2, 3).take(2).toList) // List(1, 2)
      assert( s52.take(0).toList  == s52.toList )
      assert( s52.take(-2).toList == s52.toList )
      assert( s52.take(1).toList  == s52.toList.take(1) )
      assert( s52.take(2).toList  == s52.toList.take(2) )
      assert( s52.take(9).toList  == s52.toList )
      assert( s52.take(s52.toList.length).toList  == s52.toList )
      assert( Empty.drop(1) == Empty )
      assert( s52.drop(0).toList == s52.toList )
      assert( s52.drop(1).toList == List(2, 3, 4) )
      assert( s52.drop(3).toList == List(4) )
      assert( s52.drop(4).toList == List.empty[Int])
      assert( s52.drop(5).toList == List.empty[Int])

      // Exercise 5.3
      def pred53(i: Int): Boolean = (i != 3)
      assert( Empty.takeWhile(pred53).toList == List.empty[Int] )
      assert( s52.takeWhile(pred53).toList == List(1, 2) )
      val s53 = Stream(0, 2, 4, 6, 8)
      assert( s53.takeWhile(pred53).toList == s53.toList )

      // Exercise 5.4
      def pred54(i: Int): Boolean = (i != 4)
      assert( Empty.forAll(pred54) == true )
      assert( s52.forAll(pred54) == false )
      val s54 = Stream(1, 3, 5, 7)
      assert( s54.forAll(pred54) == true )

      // // Exercise 5.5
      assert( Empty.takeWhileFr(pred53).toList == List.empty[Int] )
      assert( s52.takeWhileFr(pred53).toList == List(1, 2) )
      assert( s53.takeWhileFr(pred53).toList == s53.toList )

    }
}
