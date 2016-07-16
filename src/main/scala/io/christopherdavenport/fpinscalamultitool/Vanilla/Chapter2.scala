package io.christopherdavenport.fpinscalamultitool.Vanilla

import scala.annotation.tailrec

/**
  * Created by davenpcm on 7/2/16.
  */
object Chapter2 extends App{

  /**
    * Exercise 2.1 - Use tail recursion to generate
    *
    * This problem has a set first and second argument and can only work on the set of positive whole numbers.
    *
    * @param n The nth number of the fibonacci sequence
    * @return A number
    */
  def fib(n: Int): Int = {
    @tailrec
    def innerFib(n: Int)(first: Int, second: Int): Int = {
      val newSecond = first + second

      n match {
        case 1 => first
        case 2 => second
        case 3 => newSecond
        case _  => innerFib(n-1)(second, newSecond)
      }
    }

    n match {
      case run if n > 0 => innerFib(n)(0, 1)
    }
  }

  /**
    * Listing 2.4 Polymorphic function to find an element in an array.
    * Converted from if statements to a pattern matching block to enhance readability, and as an Option since it is
    * possible that there will be no element in the array that matches that criteria and -1 as shown in the book
    * while impossible in the scenario doesn't perfectly model that it does not exist, where Option does.
    *
    * @param as A generic array
    * @param p A predicate that if returns true will return the number of the element in the array that satisfies it
    * @tparam A The Type Parameter with no constrains
    * @return Returns an Option which contains the first value if it finds one or None if no value of the array matches
    *         the predicate.
    */
  def findFirst[A](as: Array[A], p: A => Boolean): Option[Int] = {
    @tailrec
    def loop(n: Int): Option[Int] = n match {
      case pastArrayEnd if n>= as.length => None
      case satisfiesPredicate if p(as(n)) => Some(n)
      case _ => loop(n + 1)
    }
    loop(0)
  }

  /**
    * Exercise 2.2 checks whether an array complies with a given comparison function
    *
    * @param as A generic array
    * @param ordered A comparison function of two values in the array
    * @tparam A The type parameter to be evaluated, in this case any Type
    * @return A Boolean whether or not the entire array complied
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if ( ordered(as(n),as(n + 1)) ) loop(n+ 1)
      else false
    }

    loop(0)
  }

  /**
    * This is a function that is partially applied so that initially We only give A to a function which takes A,B to
    * later be able to pass in the argument for B for the return type.
    *
    * @param a Initial Value of A
    * @param f A Function which combines A with some type and goes to another type(although all three types can also
    *          be the same.
    * @tparam A Initial Argument Type
    * @tparam B Second Argument Type
    * @tparam C Result Type
    * @return C
    */
  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  /**
    * Exercise 2.3 - This function is currying which converts a function of two arguments into a function of one
    * argument that partially applies a single parameter and the can have the other filled in later
    *
    * @param f A Function taking two values to a return value
    * @tparam A Initial Argument Type
    * @tparam B Secondary Argument Type
    * @tparam C Return Type
    * @return C
    */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  /**
    * Exercise 2.4 - Reverses the transformation of Curry
    * @param f A curryed function of A => B => C
    * @tparam A Initial Value of f function and one of supplied parameters
    * @tparam B Secondary Value of f function and one of supplied parameters
    * @tparam C return type
    * @return C
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  /**
    * Exercise 2.5 - Composes Two Function1 together
    * @param f Function B => C
    * @param g Function A => B
    * @tparam A Initial Value Type
    * @tparam B Intermediate Type Removed From Final Function
    * @tparam C Result Type
    * @return A => C
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
