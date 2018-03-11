package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A] {
  // Exercise 4.6
  // Implement versions of map, flatMap, orElse, and map2 on Either that
  // operate on the Right value.
  //def map[B](f: A => B): Either[E, B] = ???
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  //def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e)  => Left(e)
  }

  // See comment on Option.orElse.  
  //def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(b1) => Right(b1)
    case _         => b   // Answers: case Left(_) => b
  }

  // //def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  // https://www.scala-exercises.org/fp_in_scala/handling_error_without_exceptions
  // As with options, map2 lets us combine two Eithers using a binary function. 
  // // From Options Exercise 4.3
  // // Write a generic function map2 that combines two Option values
  // // using a binary function. If either Option value is None, then the
  // // return value is, too.
  // So here, if either value is Left, return Left, else return a single Right,
  // or return f applied to the contents of two Rights.
  // Here is its signature:
  //def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match{
    case Left(e)  => Left(e)
    case Right(a) => b match {
      case Left(e) => Left(e)
      case Right(b1) => Right(f(a, b1))
    }
  }
  // Answer:
  def map2A[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a,b1)
  // Desugared:
  def map2B[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (a =>
      b map (b1 =>
        f(a, b1)))
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  // Exercise 4.7 Implement sequence and traverse for Either. These should
  // return the first error that’s encountered, if there is one.

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}


object EitherExercises {
  def main(args: Array[String]): Unit = {
    // Exercise 4.6
    // Implement versions of map, flatMap, orElse, and map2 on Either that
    // operate on the Right value.

    // def map[B](f: A => B): Either[E, B]
    assert( Left("e").map((e: String) => e.length) == Left("e") )
    assert( Right("e").map((e: String) => e.length) == Right(1) )

    // def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    assert( Left("e").flatMap((e: String) => Right(e.length)) == Left("e") )
    assert( Right("e").flatMap((e: String) => Right(e.length)) == Right(1) )

    // def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    assert( Left("e").orElse(Right(42)) == Right(42) )
    assert( Right(3).orElse(Right(42))  == Right(3) )

    // def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
    def f(a: String, b: String) = a + b
    assert( Left("e").map2(Left("ee"))( f ) == Left("e") )
    assert( Left("e").map2A(Left("ee"))( f ) == Left("e") )
    assert( Left("e").map2B(Left("ee"))( f ) == Left("e") )

    assert( Left("e").map2(Right("ee"))( f ) == Left("e") )
    assert( Left("e").map2A(Right("ee"))( f ) == Left("e") )
    assert( Left("e").map2B(Right("ee"))( f ) == Left("e") )

    assert( Right("b").map2( Left("bb"))( f ) == Left("bb") )
    assert( Right("b").map2A( Left("bb"))( f ) == Left("bb") )
    assert( Right("b").map2B( Left("bb"))( f ) == Left("bb") )

    assert( Right("b").map2( Right("bb"))( f ) == Right("bbb") )
    assert( Right("b").map2A( Right("bb"))( f ) == Right("bbb") )
    assert( Right("b").map2B( Right("bb"))( f ) == Right("bbb") )


  }
}



