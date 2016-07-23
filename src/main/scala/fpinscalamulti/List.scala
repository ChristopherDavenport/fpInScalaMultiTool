package fpinscalamulti

import scala.annotation.tailrec

/**
  * Created by davenpcm on 7/3/16.
  */
sealed trait List[+A] {
  def setHead[B >: A](elem: B): List[B] = List.setHead[A, B](this)(elem)
  def init: List[A] = List.initViaFoldLeft(this)
  def tail: List[A] = List.tail(this)
  def length: Int = List.lengthViaFoldLeft(this)
  def reverse: List[A] = List.reverse(this)
  def map[B](f: A => B): List[B] = List.map(this)(f)
  def filter(f: A => Boolean): List[A] = List.filterViaFlatMap(this)(f)
  def flatMap[B](f: A => List[B]): List[B] = List.flatMap(this)(f)
  def drop(n: Int) = List.drop(this)(n)
  def dropWhile(f: A => Boolean) = List.dropWhile(this)(f)
  def append[B >: A](that: List[B]) = List.appendViaFoldLeft(this, that)
  def foldRight[B](z: B)(f: (A, B) => B): B = List.foldRightViaFoldLeft(this)(z)(f)
  def foldLeft[B](z: B)(f: (B, A) => B): B = List.foldLeft(this)(z)(f)
  def take(n: Int) = List.take(this)(n)
  def takeWhile(f: A => Boolean) = List.takeWhile(this)(f)
  def forall(f: A => Boolean) = List.forall(this)(f)
  def exists(f: A => Boolean) = List.exists(this)(f)
  def scanRight[B](z: B)(f: (A, B) => B): List[B] = List.scanRight(this)(z)(f)
  def scanLeft[B](z: B)(f: (B, A) => B): List[B] = List.scanLeft(this)(z)(f)
  def splitRight: List[List[A]] = List.splitRight(this)
  def splitLeft: List[List[A]] = List.splitLeft(this)
//  def hasSubsequence[B >: A](sub: List[B]): Boolean = List.hasSubsequence(this)(sub)
  def zip[B](that: List[B]) = List.zip(this, that)
  def zipWith[B,C](that: List[B])(f: (A, B) => C) = List.zipWith(this, that)(f)
  def ::[B >: A](elem: B): List[B] = {Cons(elem, this)}
  def :::[B >: A](that: List[B]): List[B] = that.append(this)
}
case object Nil extends List[Nothing]
case class Cons[+A](x: A, xs: List[A]) extends List[A]

object List {

  /**
    * Variadic Function Syntax to Recursively Create a List - Variadic means it accepts zero or more arguments of type
    * A
    * @param as A set of parameters all with Type A
    * @tparam A The type of the List
    * @return A List of Type A with a Nil in the furthest tail position
    */
  def apply[A](as: A*): List[A] = as match {
    case empty if empty.isEmpty => Nil
    case _ => Cons(as.head, apply(as.tail: _*))
  }

//  def sum(ints: List[Int]): Int = ints match {
//    case Nil => 0
//    case Cons(x, xs) => x + sum(xs)
//  }
//
//  def product(ds: List[Double]): Double = ds match {
//    case Nil => 1.0
//    case Cons(0.0, _) => 0.0
//    case Cons(x, xs) => x * product(xs)
//  }

  /**
    * Exercise 3.2 - Implement the function tail for removing the first element of a List. What are different choices
    * you could make in your implementation if the List is Nil
    * Choices:
    * 1) Return Nil on Nil, Pro: No errors Con: Can create infinite recursion
    * 2) Throw An Error, Pro: Means You Explicitly reach the end of the list Con: Exception Handling
    *
    * @param ls Initial List
    * @tparam A The type Parameter of the List
    * @return A new List using the same memory as the first
    */
  def tail[A](ls : List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /**
    * Exercise 3.3 - Implement the function setHead for replacing the first element of a List with a different value.
    * @param h New Head of the List
    * @param ls The list to set the new head on
    * @tparam A The type of the List
    * @return The New List which has the new Head Set
    */
  def setHead[A, B >: A](ls: List[A])(h: B): List[B] = ls match {
    case Nil => List(h)
    case Cons(x, xs) => Cons(h, xs)
  }

  /**
    * Exercise 3.4 - Generalize tail to the function drop, which removes the first n elements from a list.
    * Note - This function take time proportional only to the number of elements being dropped.
    *
    * This will create a match error on n less than zero because you cannot remove negative items from a list.
    *
    * @param l The initial list
    * @param n Number of Items to drop from the head of the list
    * @tparam A The type of the list
    * @return The new shortened list.
    */
  @tailrec
  def drop[A](l: List[A])( n: Int): List[A] = n match {
    case 0 => l
    case greaterThanZero if greaterThanZero > 0 => drop(tail(l))(n-1)
  }

  /**
    * Exercise 3.5 - Removes elements from the Lists prefix as long as the match the predicate.
    * @param l The Initial List
    * @param f The Predicate It Should Match
    * @tparam A The Type of the List
    * @return The returned list
    */
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  /**
    * This definition only copies values until the first list is exhausted so memory usage is limited by the size
    * of the initial list
    * @param a1 The First List
    * @param a2 The Second List
    * @tparam A The types of the List
    * @return A New List With all elements of both Lists
    */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * Exercise 3.6
    * My Implementation Must Go Through The Entirety of the list and when it reaches the last element it Substitutes
    * the final Cons with Nil.
    *
    * Implement a function, init, that returns a List consisting of all but the last element of a List.
    * So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    * - This implementation take n time in accordance to the size of the list. Tail uses the same list while this must
    * construct a brand new list because the initial list is memory locked to the initial block.
    *
    * @param l The List
    * @tparam A The Type of the List
    * @return A List without the last element
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def initViaFoldLeft[A](l: List[A]): List[A] = l.reverse.drop(1).reverse

  /**
    * foldRight takes a list, an initial value and a function which combines as it moves through the list
    *
    * foldRight Applies the argument so that each step from the last element is combined with the elements from
    * the tail of the list. Basically z is substituted for Nil and then the f function is applied in the place of each
    * cons. meaning it is a function per length of the list
    *
    * @param as The List
    * @param z The Initial value of the combining function
    * @param f The combining function
    * @tparam A The Initial Type of the List
    * @tparam B The Return Type of the Function
    * @return A B returned as the function
    */
  def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs)(z)(f))
  }

//  def sum2(ns: List[Int]) = foldRight(ns)(0)(_ + _)
//
//  def product2(ns: List[Double]) = foldRight(ns)(1D)(_ * _)


  /**
    * Exercise 3.9 -
    * Compute the length of a list using foldRight.
    * @param as The List
    * @tparam A The type of the list
    * @return An integer representing the length of the list
    */
  def length[A](as: List[A]): Int = foldRight(as)(0)((a, b) => b + 1)

  /**
    * Exercise 3.10 -
    * Write another general list-recursion function, foldLeft, that is tail-recursive,
    * using the techniques we discussed in the previous chapter.
    *
    * What is different about foldLeft is that it applies the argument B directly against the first argument and
    * then this accumulates to B in the next step as it shortens the list until it reaches Nil. When the list reaches
    * the Nil position it returns the value of the aggregation.
    *
    * @param as The list being aggregated
    * @param z The initial value to start the process
    * @param f A function which combines a value with the head element of the list
    * @tparam A The type of the list
    * @tparam B The return type
    * @return The result of the aggregation function
    */
  @tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs)(f(z, x))(f)
    case Nil => z
  }

  /**
    * Exercise 3.11 -
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */

  def sum(as: List[Int]): Int = foldLeft(as)(0)(_ + _)
  def product(as: List[Double]): Double = foldLeft(as)(1.0)(_ * _)
  def lengthViaFoldLeft[A](as: List[A]): Int = foldLeft(as)(0)((b, a) => b + 1)

  /**
    * Exercise 3.12 -
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    *
    * @param as The original List
    * @tparam A The Type of the List
    * @return  A new list with each element reversed
    */
  def reverse[A](as: List[A]): List[A] = foldLeft(as)(List[A]())((b: List[A], a: A) => Cons(a, b))

  def reverseViaFoldRight[A](as: List[A]): List[A] = foldRight(as)(List[A]())((a: A, b: List[A]) => append(b, List(a)))

  /**
    * Exercise 3.13 -
    *
    * This is fully in terms of FoldRight as we have implemented reverse in terms of fold right. We apply the argument
    * to the left by reversing the list and then applying the fold from the other side.
    * @param as The list
    * @param z The argument to apply to the left hand side
    * @param f The function to combine towards the end
    * @tparam A The Initial Type of the List
    * @tparam B The return type
    * @return Some value that has been aggregated
    */
  def foldLeftViaFoldRight[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldRight(reverseViaFoldRight(as))(z)((a: A , b: B) => f(b, a))
  }

  /**
    * Exercise 3.13 -
    *
    * This is fully in terms of foldLeft as reverse is also in terms of foldLeft. This function Works because Fold Right
    * Applies The combining argument at the end of the list, and foldLeft applies it to the Left hand side, so by
    * reversing the list We place the combining argument on the Right hand side and then apply till Nil and then return
    * the result.
    * @param as The list
    * @param z The argument to apply at the end of the list
    * @param f The function to aggregate the list
    * @tparam A The type of the list
    * @tparam B The return type
    * @return The aggregate of the list
    */
  def foldRightViaFoldLeft[A,B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as))(z)((b: B, a: A) => f(a, b))
  }

  /**
    * Exercise 3.14 -
    * Implement append in terms of either foldLeft or foldRight
    *
    * foldRight is the more suitable at is is in line with the way the list itself is built, starting with
    * Nil and and expanding from there - hence to build a new list from list we can build a new list and rather than
    * placing Nil at the end we place the first second list.
    * @param as The first list
    * @param bs The second list
    * @tparam A The type of both lists
    * @return A singular list merged
    */
  def appendViaFoldLeft[A](as: List[A], bs: List[A]): List[A] = {
    foldRightViaFoldLeft(as)(bs)(Cons(_, _))
  }

  /**
    * Exercise 3.15 -
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    *
    * @param ls The initial list
    * @tparam A The type of the list
    * @return A single List[A]
    */
  def concatenateToSingleList[A](ls: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(ls)(List[A]())(appendViaFoldLeft)
  }

  /**
    * Exercise 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    *
    * @param ls The Initial List
    * @return A new list with 1 added to each element
    */
  def plus1(ls: List[Int]): List[Int] = {
    foldRightViaFoldLeft(ls)(List[Int]())((a:Int, b: List[Int]) => Cons(a+1, b))
  }

  /**
    * Exercise 3.17
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    *
    * @param ls The initial list
    * @return A list of strings
    */
  def doubleListToString(ls: List[Double]): List[String] = {
    foldRightViaFoldLeft(ls)(List[String]())((a: Double, b: List[String]) => Cons(a.toString, b) )
  }

  /**
    * Exercise 3.18
    * Write a function map that generalizes modifying each element in a list
    * while maintaining the structure of the list.
    *
    * @param ls The initial list
    * @param f The transformation function
    * @tparam A The initial list type
    * @tparam B The result list type
    * @return A list of B where each element in the first list has been transformed
    */
  def map[A, B](ls : List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(ls)(List[B]())((a: A, b: List[B]) => Cons(f(a), b))
  }

  /**
    * Exercise 3.19 -
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int].
    *
    * @param as The initial list
    * @param f The filtering function
    * @tparam A The type of the list
    * @return A new list with only elements that fit the predicate
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val newList = foldLeft(as)(List[A]())((b: List[A], a: A) => f(a) match {
      case true => Cons(a, b)
      case false => b
    })
    reverse(newList)
  }

  /**
    * Exercise 3.20 -
    * Write a function flatMap that works like map
    * except that the function given will return a list instead of a single result,
    * and that list should be inserted into the final resulting list.
    * @param ls The initial list
    * @param f The transforming function
    * @tparam A The type of the initial list
    * @tparam B The type of the resulting list
    * @return A new List
    */
  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
//    foldRightViaFoldLeft( map(ls)(f) )(List[B]())(appendViaFoldLeft(_, _))
    concatenateToSingleList(map(ls)(f))
  }

  /**
    * Exercise 3.21 -
    * Use flatMap to implement filter
    *
    * The concept is fairly simple - If it fits the predicate create a list of it,
    * if it does not create a Nil list which will get removed from the final list
    *
    * @param ls the initial list
    * @param f the filtering function
    * @tparam A The type of the list
    * @return A new list with some elements removed
    */
  def filterViaFlatMap[A](ls: List[A])(f: A => Boolean): List[A] = {
    val transform : A => List[A] = {
      (a: A) => f(a) match {
        case true => List(a)
        case false => Nil
      }
    }

    flatMap(ls)(transform)
  }

  /**
    * Exercise 3.22 -
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    *
    * @param ls First List
    * @param as Second List
    * @return A New List
    */
  def addCorrespondingElements(ls: List[Int], as: List[Int]): List[Int] = {

    def internalAdder(ls: List[Int], as: List[Int], zs: List[Int]): List[Int] = (ls, as) match {
      case (Nil, _ ) => reverse(zs)
      case (_ , Nil) => reverse(zs)
      case (Cons(x, xs), Cons(y, ys)) => internalAdder(xs, ys, Cons(x + y, zs))
    }
   internalAdder(ls, as, List[Int]())
  }

  /**
    * Exercise 3.23 -
    * Generalize the function you just wrote so that it’s not specific to integers or addition.
    *
    * Since we have already written the helper function zip that returns a list of tuples of the element zipWith
    * only needs to map over the resulting tuple list with the combination function
    *
    * @param ls The first list
    * @param as The second list
    * @param f The combination function
    * @tparam A The type of the first list
    * @tparam B The type of the second list
    * @tparam C The type of the list returned
    * @return A new list of type C of all elements combined
    */
  def zipWith[A, B, C](ls: List[A], as: List[B])(f: (A, B) => C): List[C] ={
    map(zip(ls, as)){ case (a, b) => f(a, b) }
  }

  /**
    * This is a helper method for Exercise 23, both because it is in the main library and secondly it makes more
    * sense to first create a structure containing both elements and then functionally work on the aggregate than
    * combining them - Although this does mean an extra pass on the list as both operations happen seperately.
    *
    * This function is 2 pass to preserve stack safety as it could be build via Cons floating right, but
    * that would not be stack safe, so the internalZip function aggregates the list and then returns the list correctly
    * via reverse afterwards which is performed via foldLeft so the entire operation should be stack safe
    *
    * @param ls The first list
    * @param as The second list
    * @tparam A The first list type
    * @tparam B The second list type
    * @return A list that is as long as the shorter of the two lists containing a tuple of the two elements
    */
  def zip[A,B](ls: List[A], as: List[B]): List[(A, B)] = {

    @tailrec
    def internalZip(ls: List[A],
                    as: List[B],
                    zs: List[(A,B)]
                   ): List[(A, B)] = (ls, as) match {

      case (Nil, _) => reverse(zs)
      case (_ , Nil) => reverse(zs)
      case (Cons(x, xs), Cons(y, ys)) => internalZip(xs, ys, Cons((x, y), zs))
    }

    internalZip(ls, as, List[(A,B)]())
  }


  /**
    * Reinventing the wheel!
    */

  /**
    * Returns a list consisting of the n elements of the list
    * @param ls The list
    * @param n The number of elements to take
    * @tparam A The Type Of the List
    * @return A List contain N or fewer elements
    */
  def take[A](ls: List[A])(n: Int): List[A] = {
    @tailrec
    def internal(ls: List[A], n: Int, zs: List[A]): List[A] = n match {
      case 0 => reverse(zs)
      case _ => ls match {
        case Nil => reverse(zs)
        case Cons(x, xs) => internal(xs, n-1 , Cons(x, zs))
      }
    }

    internal(ls, n, List[A]())
  }

  /**
    * Returns a list consisting of the longest valid prefix whose elements all pass the predicate f
    * @param ls The List
    * @param f The predicate
    * @tparam A The type of the List
    * @return A List consisting only of elements that meet the prefix from the start of the list
    */
  def takeWhile[A](ls: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def internal(ls: List[A], zs: List[A]): List[A] = ls match {
        case Nil => reverse(zs)
        case Cons(x, xs) =>
          f(x) match {
            case true => internal(xs, Cons(x, zs))
            case false => reverse(zs)
          }
    }
    internal(ls, List[A]())
  }

  /**
    * forall returns true if and only if all elements pass the predicate f
    * First we map the list to the result of the predicate and then since all must be true to be true that
    * is the same as AND starting from a true state and any value could disqualify the whole set
    *
    * @param ls The list
    * @param f The predicate function
    * @tparam A The type of the list
    * @return A boolean representing whether every value satisfies the predicate
    */
  def forall[A](ls: List[A])(f: A => Boolean): Boolean = {
    foldLeft(map(ls)(f))(true)(_ && _)
  }

  /**
    * Returns true if any element of the list passes the predicate f
    *
    * This is the exact opposite of the for all method. It starts in the false state and any true value will make
    * the aggregate true
    *
    * @param ls The list
    * @param f The predicate function
    * @tparam A The type of the List
    * @return A boolean representing if any the the values meet the predicate
    */
  def exists[A](ls: List[A])(f: A => Boolean): Boolean = {
    foldLeft(map(ls)(f))(false)(_ || _)
  }

  def scanRight[A, B](ls: List[A])(z: B)(f: (A, B) => B): List[B] = {
    @tailrec
    def internal(ls: List[A], zs: List[B])(z: B)(f: (A, B) => B): List[B] = ls match {
      case Nil => Cons(z, zs).reverse
      case Cons(x, xs) => internal(xs, Cons(foldRightViaFoldLeft(ls)(z)(f), zs))(z)(f)
    }

    internal(ls, List[B]())(z)(f)
  }

  def splitRight[A](ls: List[A]): List[List[A]] = {
    scanRight(ls)(List[A]())(Cons(_, _)).init
  }

  def splitLeft[A](ls: List[A]): List[List[A]] = {
    scanRight(ls.reverse)(List[A]())(Cons(_, _)).map(_.reverse).reverse.tail
  }

  def scanLeft[A, B](ls: List[A])(z: B)(f: (B, A) => B): List[B] = {
    scanRight(ls.reverse)(List[A]())(Cons(_, _)).reverse.map(_.foldLeft(z)(f))
  }

  /**
    * Exercise 3.24 -
    * While it would be considerably harder to generate all subsequences, finding each is merely taking each type
    * and gaining subsequences of the same length and them matching them against the given subsequence.
    *
    * @param sup The list to search
    * @param sub The subsequence to search for
    * @tparam A The type of the sequences
    * @return A boolean whether the given subsequence exists or not
    */
  def hasSubsequence[A, B >: A](sup: List[A])(sub: List[B]): Boolean = {
    splitRight(sup).map(_.take(sub.length)).exists(_ == sub)
  }




}
