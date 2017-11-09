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
  def getOrElse[B>:A](default: => B): B = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def orElse[B>:A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
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

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}


object OptionExercises {
  def main(args: Array[String]): Unit = {
    val someInt = Some(42)

    // Ex. 4.1 Implement all of the preceding functions on Option

    // Implement def map[B](f: A => B): Option[B]
    assert( Some(42).map( x => x + 1) == Some(43) )

    // Implement  def getOrElse[B>:A](default: => B): B = ???

    // Implement  def flatMap[B](f: A => Option[B]): Option[B] = ???

    // Implement  def orElse[B>:A](ob: => Option[B]): Option[B] = ???

    // Implement  def filter(f: A => Boolean): Option[A] = ???

    // // Ex. 3.2, implement tail.
    // assert(List.tail(l) match {
    //   case Cons(2, Cons(3, Nil)) => true
    //   case _ => false
    // })

  }
}
