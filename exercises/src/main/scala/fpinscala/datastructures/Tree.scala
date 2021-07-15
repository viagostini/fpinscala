package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = 
    fold(tree)(a => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int = 
    fold(tree)(a => a)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int = 
    fold(tree)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = 
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}

object TreeTests extends App {
  val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  println(Tree.size(tree))
  println(Tree.maximum(tree))
  println(Tree.depth(tree))
  println(Tree.map(tree)(_ * 2.0))
}
