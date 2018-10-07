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

  // Exercise 5.6 Hard: Implement headOption using foldRight. 
  //def headOption: Option[A] = ???
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }
  def headOptionFR: Option[A] = foldRight(None: Option[A])((h, t) => Some(h))

  // Exercise 5.7 Implement map, filter, append, and flatMap using foldRight.
  // Part of the exercise is writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    this.foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  // About [B>:A].  Up at the top we have "trait Stream[+A]", so that Stream is
  // covariant in A.  This means that if A is a subtype of B, then Stream[A] is
  // a subtype of Stream[B].  If we append Stream[B] to Stream[A], we would
  // expect that the methods we could call on the result would apply to both A
  // and B, so we require that B be a parent of A; i.e., A is a lower type
  // bound for B.
  // https://docs.scala-lang.org/tour/lower-type-bounds.html
  // https://www.atlassian.com/blog/software-teams/covariance-and-contravariance-in-scala
  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))

  // Exercise 5.14 Hard: Implement startsWith using functions you’ve written.
  // It should check if one Stream is a prefix of another. For instance,
  // Stream(1,2,3) startsWith Stream(1,2) would be true.
  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


/**  
  * 
  * The companion object.  
  * 
  */
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

  // Exercise 5.8  Generalize ones slightly to the function constant, which
  // returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  // Answer: This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9 Write a function that generates an infinite stream of
  // integers, starting from n, then n + 1, n + 2, and so on.  Note that
  // in Scala, the Int type is a 32-bit signed integer, so this stream will
  // switch from positive to negative values at some point, and will repeat
  // itself after about four billion elements.
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Exercise 5.10  Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def adder(iPrev: Int, iCurr: Int): Stream[Int] = {
      //cons(iPrev, adder(iPrev + iCurr, iPrev))
      // or
      cons(iPrev, adder(iCurr, iPrev + iCurr))
    }
    adder(0, 1)
  }

  // Exercise 5.11  Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
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

      // Exercise 5.5
      assert( Empty.takeWhileFr(pred53).toList == List.empty[Int] )
      assert( s52.takeWhileFr(pred53).toList == List(1, 2) )
      assert( s53.takeWhileFr(pred53).toList == s53.toList )

      // Exercise 5.6
      assert( Empty.headOption == None )
      assert( s52.headOption == Some(1) )
      assert( Empty.headOptionFR == None )
      assert( s52.headOptionFR == Some(1) )

      // Exercise 5.7 map
      val s57 = Stream(1, 2, 3)
      def f57(i: Int): String = "I" * i
      assert( Empty.map(f57).toList == List.empty[String] )
      assert( s57.map(f57).toList == List("I", "II", "III") )

      // Exercise 5.7 filter
      def p57(i: Int): Boolean = (0 == i % 2)
      assert( Empty.filter(p57).toList == List.empty[Int])
      assert( s57.filter(p57).toList   == List(2) )

      // Exercise 5.7 append
      assert( Empty.append(s57).toList == s57.toList )
      assert( s52.append(s57).toList == s52.toList ::: s57.toList )

      // Exercise 5.7 flatMap
      def f57b(i: Int): Stream[String] =
        Stream((1 to i).map(j => "I" * j).toList :_*)
      assert( Empty.flatMap(f57b).toList == List.empty[String] )
      //println( s57.flatMap(f57b).toList )
      ( s57.flatMap(f57b).toList == List("I", "I", "II", "I", "II", "III") )

      // Exercise 5.8
      assert( Stream.constant("a").take(4).toList == List("a", "a", "a", "a") )
    
      // Exercise 5.9
      assert( Stream.from(4).take(3).toList == List(4, 5, 6) )

      // Exercise 5.10
      assert( fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) )



    }
}




