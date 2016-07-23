package fpinscalamulti

/**
  * Created by davenpcm on 7/4/16.
  */
sealed trait Tree[+A]{
  lazy val size = Tree.size(this)
  lazy val depth = Tree.depth(this)
  def map[B](f: A => B) = Tree.map(this)(f)
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25 -
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    *
    * @param t The tree to be measured
    * @tparam A The type of the tree
    * @return The size of all elements in the tree
    */
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => size(left) + size(right) + 1
    case Leaf(value) => 1
  }

  /**
    * Exercise 3.26 -
    * Write a function maximum that returns the maximum element in a Tree[Int]
    *
    * @param t The tree to evalue
    * @return An int corresponding to the largest element in the tree
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
  }

  /**
    * Exercise 3.27 -
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    *
    * @param t The Tree
    * @tparam A The type of the Tree
    * @return An integer representing the depth of the tree
    */
  def depth[A](t: Tree[A]): Int =  {

    def internal(t: Tree[A], z: Int): Int = t match {
      case Branch(l, r) => internal(l, z+1) max internal(r, z+1)
      case Leaf(v) => z + 1
    }

    internal(t, 0)
  }

  /**
    * Exercise 3.28 -
    * Write a function map, analogous to the method of the same name on List,
    * that modifies each element in a tree with a given function.
    *
    * @param t The Tree
    * @param f The function to transform
    * @tparam A  The initial type of the Tree
    * @tparam B The return type of the new tree
    * @return The Tree
    */
  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Branch(l, r) => Branch[B]( map(l)(f), map(r)(f) )
    case Leaf(v) => Leaf[B](f(v))
  }

  /**
    * Exercise 3.29 -
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
    * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function
    * and the left and right folds for List?
    *
    * Fold takes something to transform leaves and something to transform branches as the operate differently
    *
    * @param t The initial tree
    * @param f The transformation function for leaves
    * @param g The transformation function for branches
    * @tparam A The Initial Type of the Tree
    * @tparam B The return type
    * @return A value of type B
    */
  def fold[A, B](t: Tree[A])(f:  A => B)(g: (B, B) => B): B  ={
    def internal(t: Tree[A])(f:  A => B)(g: (B, B) => B): B = t match {
      case Branch(l, r) => g( internal(l)(f)(g), internal(r)(f)(g)   )
      case Leaf(v) => f(v)
    }
    internal(t)(f)(g)
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((ba: Int, bb: Int) => ba + bb + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)( _ max _ )

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)((ba, bb) =>  (ba max bb) + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}