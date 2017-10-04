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

    // Ex. 3.26 Write maximum() for a Tree[Int].
    assert(Tree.maximum(iT) == 5)

    // Ex. 3.27 Write depth.
    assert(Tree.depth(sT) == 2)
    assert(Tree.depth(iT) == 3)



  }

}
