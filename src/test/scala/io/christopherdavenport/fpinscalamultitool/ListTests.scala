package io.christopherdavenport.fpinscalamultitool

import org.scalatest.{FlatSpec, Matchers}
import List._

/**
  * Created by davenpcm on 7/3/16.
  */
class ListTests extends FlatSpec with Matchers{

  "apply" should "create a List" in {
    val list = List(1, 2, 3)
    list should be ( Cons(1, Cons(2, Cons(3, Nil))) )
  }

  it should "create a list with no arguments if given a type" in {
    val l : List[Int] = List()
    l should be  ( Nil )
  }

  "tail" should "return the tail of the list" in {
    val l = List(1, 2, 3, 4, 5)

    tail(l) should be (List(2, 3, 4, 5))
  }

  it should "return Nil At The End of a List" in {
    val l = List(1)
    tail(l) should be (Nil)
  }

  it should "return Nil on a Nil List" in {
    val l : List[Int] = List()
    tail(l) should be (Nil)
  }

  "setHead" should "add a new Head to a List" in {
    val l = List("I've", "Got", "Game")
    setHead(l)("You've") should be (List("You've", "Got", "Game"))
  }

  it should "add an Element to an Empty List" in {
    val l : List[Int] = Nil

    setHead(l)(1) should be (List(1))
  }

  "drop" should "drop a number of elements from a list" in {
    val l = List(1, 2, 3, 4, 5)

    drop(l)(2) should be (List(3, 4, 5))
  }

  it should "return Nil if taken past the end of the list" in {
    val l = List(1,2,3)
    drop(l)(5) should be (Nil)
  }

  it should "return Nil if taken on a Nil list" in {
    val l : List[Int] = Nil
    drop(l)(9) should be (Nil)
  }

  "dropWhile" should "drop Elements while a predicate is met" in {
    val l = List(1, 2, 3, 4, 5)
    val f = (i: Int) => i <= 3
    dropWhile(l)(f) should be (List(4, 5))
  }

  it should "return the intial list if no elements satisfy the predicate" in {
    val l = List(1,2,3,4,5)
    val f = (i: Int) => false
    dropWhile(l)(f) should be (l)
  }

  it should "return Nil if all elements satisfy the predicate" in {
    val l = List(1,2,3,4,5)
    val f = (i: Int) => true
    dropWhile(l)(f) should be (Nil)
  }

  it should "return Nil if the initial List is Nil" in {
    val l : List[Int] = Nil
    val f = (i: Int) => true
    dropWhile(l)(f) should be (Nil)
  }

  "init" should "remove the last element from a list" in {
    val l = List(1,2,3,4)
    init(l) should be (List(1,2,3))
  }

  it should "return Nil if the original list was Nil" in {
    val l: List[Int] = Nil

    init(l) should be (Nil)
  }

  "foldRight" should "return the same list if passed Nil and Cons" in {
    val l = List(1,2,3,4,5)

    foldRight(l)(Nil: List[Int])(Cons(_, _)) should be (l)
  }

  it should "be able to create the sum function" in {
    val l = List(1,2,3,4,5)

    foldRight(l)(0)((a, b) => a + b) should be (List.sum(l))
  }

  "length" should "return the length of the list" in {
    val l = List(1,2,3,4,5)

    List.length(l) should be (5)
  }

  it should "return 0 for an empty list" in {
    val l : List[Int] = Nil
    List.length(l) should be (0)
  }

  "sum" should "return the sum of a list" in {
    val l = List(1,2,3,4,5)
    List.sum(l) should be (15)
  }

  it should "return zero on an Empty list" in {
    val l = List[Int]()
    List.sum(l) should be (0)
  }

  "product" should "return the product of a list of doubles" in {
    val l = List(1.0, 2.0, 3.0)

    List.product(l) should be (6D)
  }

  it should "return 1 for an empty list" in {
    val l = List[Double]()
    List.product(l) should be (1D)
  }

  "lengthViaFoldLeft" should "produce the length of a list" in {
    val l = List(1,2,3,4,5)

    List.lengthViaFoldLeft(l) should be (5)
  }

  it should "produce a length of zero for an empty list" in {
    val l = List[String]()
    List.lengthViaFoldLeft(l) should be (0)
  }

  "reverse" should "reverse a list" in {
    val l = List(1,2,3,4,5)
    List.reverse(l) should be (List(5,4,3,2,1))
  }

  "reverseViaFoldRight" should "reverse a list" in {
    val l = List(1,2,3,4,5)
    List.reverseViaFoldRight(l) should be (List(5,4,3,2,1))
  }

  "foldLeftViaFoldRight" should "implement the same function as foldLeft" in {
    val l = List(1,2,3,4,5)
    foldLeftViaFoldRight(l)(0)(_ + _) should be ( foldLeft(l)(0)(_ + _) )
  }

  "foldRightViaFoldLeft" should "implement create the same function as foldRight" in {
    val l = List(1,2,3,4,5)
    foldRightViaFoldLeft(l)(0)(_ + _) should be (foldRight(l)(0)(_ + _))
  }

  "appendViaFoldLeft" should "concatenate two lists" in {
    val l1 = List(1,2,3)
    val l2 = List(4, 5)
    appendViaFoldLeft(l1, l2) should be (List(1,2,3,4,5))
  }

  "concatenateToSingleList" should "combine a list of lists to a single list" in {
    val l1 = List(1, 2)
    val l2 = List(3, 4)
    val l3 = List(5, 6)
    val l4 = List(7, 8)
    val l5 = List(9, 10)
    val l = List(l1, l2, l3, l4, l5)

    concatenateToSingleList(l) should be (List(1,2,3,4,5,6,7,8,9,10))
  }

  it should "remove Nil lists if found" in {
    val l1 = List(1,2)
    val l2 = List[Int]()
    val l3 = List(3,4)
    val l4 = List[Int]()
    val l5 = List(5)
    val l = List(l1, l2, l3, l4 ,l5)

    concatenateToSingleList(l) should be (List(1,2,3,4,5))
  }

  "plus1" should "add 1 to each element in a list" in {
    val l = List(1,2,3,4,5)

    plus1(l) should be (List(2,3,4,5,6))
  }

  "doubleListToString" should "convert a list of double to a list of strings" in {
    val l = List(1.0, 2.0, 3.0)

    doubleListToString(l) should be (List("1.0", "2.0", "3.0"))
  }

  "map" should "be able to add 1 to each element in a list of int" in {
    val l = List(1,2,3,4,5)
    val f: (Int => Int) = _ + 1

    map(l)(f) should be (List(2,3,4,5,6))
  }

  it should "be able to convert a list of double to a list of strings" in {
    val l = List(1.0, 2.0, 3.0)
    val f: (Double => String) = _.toString

    map(l)(f) should be (List("1.0", "2.0", "3.0"))
  }

  "filter" should "remove elements that do not fit a predicate" in {
    val l = List(1,2,3,4,5)
    val f: (Int => Boolean) = _ >= 3

    filter(l)(f) should be (List(3, 4, 5))
  }

  "flatMap" should "take a function with creates lists from the type of the list to create a single list" in {
    val l = List(1,2,3)
    val f : (Int => List[Int]) = (i: Int) => List(i, i)

    flatMap(l)(f) should be (List(1,1,2,2,3,3))
  }

  "filterViaFlatMap" should "remove elements that do not fit a predicate" in {
    val l = List(1,2,3,4,5)
    val f: (Int => Boolean) = _ >= 3

    filterViaFlatMap(l)(f) should be (List(3,4,5))
  }

  "addCorrespondingElements" should "add together values of two lists of integers" in {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)

    addCorrespondingElements(l1, l2) should be (List(5, 7, 9))
  }

  it should "only zip to the length of the shorter of the two lists" in {
    val l1 = List(1,2,3,4,5)
    val l2 = List(4,5,6)
    addCorrespondingElements(l1, l2) should be (List(5,7,9))
  }

  "zip" should "return a tuple of the elements of two lists" in {
    val l1 = List(1,2,3)
    val l2 = List("I", "am", "awesome!")

    zip(l1, l2) should be (List((1, "I"), (2, "am"), (3, "awesome!")))
  }

  it should "only zip to the length of the shorter of the two lists" in {
    val l1 = List(1,2,3,4,5,6,7)
    val l2 = List("I", "am", "awesome!")

    zip(l1, l2) should be (List((1, "I"), (2, "am"), (3, "awesome!")))
  }

  "zipWith" should "be able to add all elements in lists of integers" in {
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val f : (Int, Int) => Int = _ + _

    zipWith(l1, l2)(f) should be (List(5, 7, 9))
  }

  it should "be able to generate new types from the combination" in {
    val l1 = List(1.0, 2.0, 3.0)
    val l2 = List(4, 5, 6)
    val f : (Double, Int) => String = (d: Double, i: Int) =>  s"${(d.toInt * i).toString}!"

    zipWith(l1, l2)(f) should be (List("4!", "10!", "18!"))
  }

  "take" should "only take the number of elements indicated from the list" in {
    val l1 = List(1,2,3,4,5)

    take(l1)(3) should be (List(1,2,3))
  }

  it should "return Nil if the list is Nil" in {
    val l1 = List[Int]()

    take(l1)(3) should be (List[Int]())
  }

  it should "return the entire list if n is larger than the length of the list" in {
    val l1 = List(1,2,3,4,5)

    take(l1)(12) should be (List(1,2,3,4,5))
  }

  "takeWhile" should "return the first elements that pass the test" in {
    val l1 = List(1,2,3,4,5)
    val p : (Int => Boolean) = _ <= 3

    takeWhile(l1)(p) should be (List(1,2,3))
  }

  it should "return a Nil List if the first element does not pass the predicate" in {
    val l1 = List(1,2,3,4,5)
    val p : (Int => Boolean) = _ > 5

    takeWhile(l1)(p) should be (List[Int]())
  }

  "forall" should "return true if all elements match a predicate" in {
    val l1 = List(1,2,3,4,5)
    val p : (Int => Boolean) = _ <= 6

    forall(l1)(p) should be (true)
  }

  it should "fail if a single element does not match" in {
    val l1 = List(1,2,3,4,5)
    val p : (Int => Boolean) = _ < 5

    forall(l1)(p) should be (false)
  }

  "exists" should "return true if a single element matches the predicate" in {
    val l1 = List(1,2,3,4,5)
    val p : (Int => Boolean) = _ == 4

    exists(l1)(p) should be (true)
  }

  it should "return false if none of the elements satisfies the predicate" in {
    val l1 = List(1,2,3,4,5)
    val p: (Int => Boolean) = _ == 27

    exists(l1)(p) should be (false)
  }

  it should "return true if they all match the predicate" in {
    val l1 = List(1,2,3,4,5)
    val p: (Int => Boolean) = _ < 10

    exists(l1)(p) should be (true)
  }

  "scanRight" should "return successive pieces of the iteration" in {
    val l1 = List(1,2,3)


    scanRight(l1)(0)(_ + _) should be (List(6,5,3,0))
  }

  "scanLeft" should "retrun small pieces of the iteration" in {
    val l1 = List(1,2,3,4,5)

    scanLeft(l1)(0)(_ + _) should be (List(0, 1, 3, 6, 10, 15))
  }

  "splitRight" should "return each Cons as its own list" in {
    val l1 = List(1,2,3,4,5)

    splitRight(l1) should be (List(List(1,2,3,4,5), List(2,3,4,5), List(3,4,5), List(4,5), List(5)))
  }

  "splitLeft" should "return a list of each list starting from the left" in {
    val l1 = List(1,2,3,4,5)

    splitLeft(l1) should be (List(List(1), List(1,2), List(1,2,3), List(1,2,3,4), List(1,2,3,4,5)))
  }

  "hasSubsequence" should "return whether a given subsequence is present in the list" in {
    val l1 = List(1,2,3,4,5)
    val l2 = List(3,4)

    hasSubsequence(l1)(l2) should be (true)
  }

}
