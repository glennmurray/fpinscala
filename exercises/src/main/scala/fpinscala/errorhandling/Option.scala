package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`,
// since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _} 

sealed trait Option[+A] {
  // Ex. 4.1 Implement all of these functions on Option
  //def map[B](f: A => B): Option[B] = ???
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _       => None
  }

  // The function getOrElse tries to get the value contained
  // in the Option, but if it's a None, it will return the default
  // value provided by the caller.
  // The "default: => B" indicates that the argument is
  // of type B, but won’t be evaluated until it’s needed
  // by the function.
  //def getOrElse[B>:A](default: => B): B = ???
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _       => default
  }

  //def flatMap[B](f: A => Option[B]): Option[B] = ???
  // def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //   case Some(a) => f(a)
  //   case _       => None
  // }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // orElse returns the original Option if not None, or returns
  // the provided Option as an alternative in that case.
  //def orElse[B>:A](ob: => Option[B]): Option[B] = ???
  def orElse[B>:A](ob: => Option[B]): Option[B] = ob

  // Convert Some to None if the value does not satisfy f.
  //def filter(f: A => Boolean): Option[A] = ???
  // def filter(f: A => Boolean): Option[A] = this match {
  //   case Some(a) if f(a) => Some(a)
  //   case _               => None
  // }
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`,
    // and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones
    // we've seen.
    // `case e: Exception` is a pattern that matches any `Exception`,
    // and it binds this value to the identifier `e`. The match
    // returns the value 43.
    catch { case e: Exception => 43 } 
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  // Exercise 4.2 Implement the variance function in terms of flatMap.
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2)) ))
  }

  // Exercise 4.3
  // Write a generic function map2 that combines two Option values
  // using a binary function. If either Option value is None, then the
  // return value is, too. Here is its signature:
  def map2A[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => println(s"YO $a $b"); Some(f(a, b))
      case _                  => None
    }
  // With flatMap.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap( x => b.map(y => f(x, y)) )

  // Exercise 4.4
  // Write a function sequence that combines a list of Options into one
  // Option containing a list of all the Some values in the original list.
  // If the original list contains None even once, the result of the
  // function should be None; otherwise the result should be Some with a
  // list of all the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case ho :: to => ho.flatMap(oa => sequence(to) map (oa :: _))
  }

  // Exercise 4.5 Implement this function. It’s straightforward to do
  // using map and sequence... 
  // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
  def traverse0[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map( f(_) ))
  // ...but try for a more efficient implementation that only looks at
  // the list once.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => f(h) flatMap (x => traverse(t)(f).map(x :: _))
      // Answers use map2, 
      //case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  // and foldRight:
  //a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  // In fact, implement sequence in terms of traverse.
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}


object OptionExercises {
  def main(args: Array[String]): Unit = {

    // Ex. 4.1 Implement all of the preceding functions on Option

    // Implement def map[B](f: A => B): Option[B]
    assert( Some(42).map( x => x + 1) == Some(43) )

    // Implement  def getOrElse[B>:A](default: => B): B
    assert( Some(42).getOrElse("else") == 42 )
    assert( None.getOrElse("else") == "else" )

    // Implement  def flatMap[B](f: A => Option[B]): Option[B]
    assert( Some("Odessa").flatMap( x => Some(x.length)) == Some(6) )
    assert( Some(42).flatMap( x => Some(x + 1)) == Some(43) )
    assert( Some(42).flatMap( x => None) == None )


    // Implement  def orElse[B>:A](ob: => Option[B]): Option[B]
    val x = None.orElse(Some(41)).orElse(None).orElse(Some(42))
    //println(s"*** x = $x")
    assert( x == Some(42) )

    // Implement  def filter(f: A => Boolean): Option[A]
    // Convert Some to None if the value does not satisfy f.
    def even(i: Int): Boolean = i % 2 == 0
    assert( Some(42).filter(even) == Some(42) )
    assert( Some(42).filter(i => i % 2 == 0) == Some(42) ) // Anon. function
    assert( Some(41).filter(even) == None ) 
    assert( None.filter(even) == None ) 

    // Exercise 4.2 Implement the variance function in terms of flatMap.
    val xs: Seq[Double] = Seq(1, 2, 3, 4)
    assert( fpinscala.errorhandling.Option.variance(xs) == Some(1.25) )

    // Exercise 4.3
    // Write a generic function map2 that combines two Option values
    // using a binary function. If either Option value is None, then the
    // return value is, too. Here is its signature:
    //def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C].
    def sumf(i: Int, j: Int): Int = i + j
    assert( fpinscala.errorhandling.Option.map2(
      Some(2), Some(3))( sumf ) == Some(5) )
    // This won't work.
    // assert( fpinscala.errorhandling.Option.map2(
    //   None, None)( sumf ) == None )
    val xN: Option[Int] = None
    val yN: Option[Int] = None
    assert( fpinscala.errorhandling.Option.map2(
      xN, Some(3))( sumf ) == None )
    assert( fpinscala.errorhandling.Option.map2(
      xN, yN)( sumf ) == None )
    assert( fpinscala.errorhandling.Option.map2A(
      xN, Some(8))( sumf ) == None )

    // Exercise 4.4
    // Write a function sequence that combines a list of Options into one
    // Option containing a list of all the Some values in the list.
    // If the original list contains None even once, the result of the
    // function should be None; otherwise the result should be Some with a
    // list of all the values.
    //   def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
    assert (fpinscala.errorhandling.Option
      .sequence(List(Some(2), Some(4), Some(6))) == Some(List(2, 4, 6)))
    assert (fpinscala.errorhandling.Option
      .sequence(List(None, Some(4), Some(6)))    == None)
    assert (fpinscala.errorhandling.Option
      .sequence(List(Some(2), None, Some(6)))    == None)

    // Exercise 4.5 Implement this function. It’s straightforward to do
    // using map and sequence...
    // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
    assert(fpinscala.errorhandling.Option
      .traverse0(List(1, 2, 3))(x => Some(x * 2)) == Some(List(2, 4, 6)))
    assert(fpinscala.errorhandling.Option
      .traverse0(List(1, 2, 3))(x => None) == None)
    // ... but try for a more efficient implementation that only looks at
    // the list once.
    assert(fpinscala.errorhandling.Option
      .traverse(List(1, 2, 3))(x => Some(x * 2)) == Some(List(2, 4, 6)))
    assert(fpinscala.errorhandling.Option
      .traverse(List(1, 2, 3))(x => None) == None)
    // In fact, implement sequence in terms of traverse.
    assert (fpinscala.errorhandling.Option
      .sequenceViaTraverse(List(Some(2), Some(4), Some(6))) ==
      Some(List(2, 4, 6)))
    assert (fpinscala.errorhandling.Option
      .sequenceViaTraverse(List(None, Some(4), Some(6)))    == None)
    assert (fpinscala.errorhandling.Option
      .sequenceViaTraverse(List(Some(2), None, Some(6)))    == None)
  }
}
