package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Ex. 3.25 Write a function size that counts the number
  // of nodes (leaves and branches) in a tree.
  def size[A](t: Tree[A]): Int = t match {
    case l: Leaf[A] => 1
    case b: Branch[A] => 1 + size(b.left) + size(b.right)
  }

  // Ex. 3.26 Write a function maximum that returns the maximum
  // element in a Tree[Int].
  // NOTE if you write maximum[A] it won't compile, because the compiler
  // thinks that max is supposed to apply to a type parameter, not Int.
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Ex. 3.27 Write a function depth that returns the maximum path
  // length from the root of a tree to any leaf.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0 // Why not count the leaf?  Because we count the root.
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Ex 3.28.  Write a function map, analogous to "map" List,
  // that modifies each element in a tree with a given function.
  //def map[A,B](l: Tree[A])(f: A => B): Tree[B] = ???
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Ex. 3.29.  Generalize size, maximum, depth, and map, writing a new
  // function fold that abstracts over their similarities. Reimplement
  // them in terms of this more general function.
  //
  // One would think fold could be implemented like List,
  //   def fold[A,B](t: Tree[A], z: B)(f: (B, A) => B): B = ???
  // but they have something else in mind: a "handler" for each of the
  // data constructors of the type.  So,
  //   def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = ???
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g( fold(l)(f)(g), fold(r)(f)(g) )
  }

//  def foldSize[A](t: Tree[A]): Int = fold(t)(a => 1)((b1, b2) => 1 + b1 + b2)
  // Better:
  def foldSize[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def foldMaximum(t: Tree[Int]): Int = fold(t)(i => i)(_ max _)

  def foldDepth[A](t: Tree[A]): Int = fold(t)(i => 0)(
    (b1, b2) => 1 + (b1 max b2))
  // This won't compile (!--compare to foldSize) because the compiler needs
  // to know for max that the underscores are Ints.
  //def foldDepth[A](t: Tree[A]): Int = fold(t)(i => 0)((_ max _) + 1)

  def foldMap[A,B](t: Tree[A])(f: A => B): Tree[B] =
    // This Leaf needs a Tree type annotation because otherwise the compiler
    // infers the type of B in the fold def to be Leaf[B], which breaks g.
    //fold(t)(a => Leaf(f(a))( (b1, b2) => Branch(b1, b2) )
    fold(t)(a => Leaf(f(a)): Tree[B])( (b1, b2) => Branch(b1, b2) )



}


object TreeExercises {
  // This is the tree in Section 3.5.
  val sT: Tree[String] = Branch(
    Branch(Leaf("a"), Leaf("b")),
    Branch(Leaf("c"), Leaf("d")))

  /*
            B
          /   \
         B     B
        / \   / \
       1   2 3   B
                / \
               4   5
   */
  val iT: Tree[Int] = Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
  )

  def main(args: Array[String]): Unit = {

    // Ex. 3.25 Write a function size that counts the number
    // of nodes (leaves and branches) in a tree.
    assert(Tree.size(sT) == 7)
    assert(Tree.size(iT) == 9)

    // Ex. 3.26 Write maximum() for a Tree[Int].
    assert(Tree.maximum(iT) == 5)

    // Ex. 3.27 Write depth.
    assert(Tree.depth(sT) == 2)
    assert(Tree.depth(iT) == 3)

    // Ex 3.28.  Write map.
    assert( Tree.map(sT)((s: String) => s + s.toUpperCase) match {
      case Branch(Branch(Leaf(a), Leaf(b)), Branch(Leaf(c), Leaf(d))) =>
        a == "aA" && b == "bB" && c == "cC" && d == "dD"
      case _ => false
    })

    // Ex. 3.29. Write fold, and use it in size, maximum, depth, and map.
    assert(Tree.fold(iT)(i => i)(_ + _) == 15)
    assert(Tree.fold(sT)(a => a)(_ + _) == "abcd")

    assert(Tree.foldSize(sT) == 7)
    assert(Tree.foldSize(iT) == 9)

    assert(Tree.foldMaximum(iT) == 5)

    assert( Tree.foldMap(sT)((s: String) => s + s.toUpperCase) match {
      case Branch(Branch(Leaf(a), Leaf(b)), Branch(Leaf(c), Leaf(d))) =>
        a == "aA" && b == "bB" && c == "cC" && d == "dD"
      case _ => false
    })
  }

}
