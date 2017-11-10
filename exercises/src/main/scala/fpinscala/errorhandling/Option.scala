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
  //def variance(xs: Seq[Double]): Option[Double] = ???
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2)) ))
  }


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
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
    assert( Some(41).filter(even) == None ) 
    assert( None.filter(even) == None ) 

    // Exercise 4.2 Implement the variance function in terms of flatMap.
    val xs: Seq[Double] = Seq(1, 2, 3, 4)
    assert( fpinscala.errorhandling.Option.variance(xs) == Some(1.25) )
  }
}
