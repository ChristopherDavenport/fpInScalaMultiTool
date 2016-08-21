package fpinscalamulti.Vanilla.datastructures

import fpinscalamulti.Vanilla.datastructures
import fpinscalamulti.Vanilla.datastructures.List._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by davenpcm on 7/3/16.
  */
class ListTests extends FlatSpec with Matchers{

  "apply" should "create a List" in {
    val list = datastructures.List(1, 2, 3)
    list should be ( Cons(1, Cons(2, Cons(3, Nil))) )
  }

  it should "create a list with no arguments if given a type" in {
    val l : List[Int] = datastructures.List()
    l should be  ( datastructures.Nil )
  }

  "tail" should "return the tail of the list" in {
    val l = datastructures.List(1, 2, 3, 4, 5)
    l.tail should be (datastructures.List(2, 3, 4, 5))
  }

  it should "return Nil At The End of a List" in {
    val l = datastructures.List(1)
    l.tail should be (datastructures.Nil)
  }

  it should "return Nil on a Nil List" in {
    val l : List[Int] = datastructures.List()
    l.tail should be (datastructures.Nil)
  }

  "setHead" should "add a new Head to a List" in {
    val l = datastructures.List("I've", "Got", "Game")
    l.setHead("You've") should be (datastructures.List("You've", "Got", "Game"))
  }

  it should "add an Element to an Empty List" in {
    val l : List[Int] = datastructures.Nil
    l.setHead(1) should be (datastructures.List(1))
  }

  "drop" should "drop a number of elements from a list" in {
    val l = datastructures.List(1, 2, 3, 4, 5)
    l.drop(2) should be (datastructures.List(3, 4, 5))
  }

  it should "return Nil if taken past the end of the list" in {
    val l = datastructures.List(1,2,3)
    l.drop(5) should be (datastructures.Nil)
  }

  it should "return Nil if taken on a Nil list" in {
    val l : List[Int] = datastructures.Nil
    l.drop(9) should be (datastructures.Nil)
  }

  "dropWhile" should "drop Elements while a predicate is met" in {
    val l = datastructures.List(1, 2, 3, 4, 5)
    val f = (i: Int) => i <= 3
    l.dropWhile(f) should be (datastructures.List(4, 5))
  }

  it should "return the intial list if no elements satisfy the predicate" in {
    val l = datastructures.List(1,2,3,4,5)
    val f = (i: Int) => false
    l.dropWhile(f) should be (l)
  }

  it should "return Nil if all elements satisfy the predicate" in {
    val l = datastructures.List(1,2,3,4,5)
    val f = (i: Int) => true
    l.dropWhile(f) should be (datastructures.Nil)
  }

  it should "return Nil if the initial List is Nil" in {
    val l : List[Int] = datastructures.Nil
    val f = (i: Int) => true
    l.dropWhile(f) should be (datastructures.Nil)
  }

  "init" should "remove the last element from a list from the list" in {
    val l = datastructures.List(1,2,3,4)
    l.init should be (datastructures.List(1,2,3))
  }

  it should "return Nil if the original list was Nil" in {
    val l: List[Int] = datastructures.Nil
    datastructures.List.init(l) should be (datastructures.Nil)
  }

  it should "return nil with a single element" in {
    val l  = datastructures.List(1)
    datastructures.List.init(l) should be (datastructures.Nil)
  }

  it should "remove the last element from a list" in {
    val l = datastructures.List(1,2,3,4)
    datastructures.List.init(l) should be (datastructures.List(1,2,3))
  }

  "foldRight" should "return the same list if passed Nil and Cons" in {
    val l = datastructures.List(1,2,3,4,5)
    l.foldRight(datastructures.Nil: List[Int])(Cons(_, _)) should be (l)
  }

  it should "be able to create the sum function" in {
    val l = datastructures.List(1,2,3,4,5)

    l.foldRight(0)((a, b) => a + b) should be (datastructures.List.sum(l))
  }

  "length" should "return the length of the list" in {
    val l = datastructures.List(1,2,3,4,5)
    l.length should be (5)
  }

  it should "return 0 for an empty list" in {
    val l : List[Int] = datastructures.Nil
    datastructures.List.length(l) should be (0)
  }

  it should "return the length of the list when using the object" in {
    val l = List(1,2,3,4,5)
    List.length(l) should be (5)
  }

  "sum" should "return the sum of a list" in {
    val l = datastructures.List(1,2,3,4,5)
    datastructures.List.sum(l) should be (15)
  }

  it should "return zero on an Empty list" in {
    val l = datastructures.List[Int]()
    datastructures.List.sum(l) should be (0)
  }

  "product" should "return the product of a list of doubles" in {
    val l = datastructures.List(1.0, 2.0, 3.0)
    datastructures.List.product(l) should be (6D)
  }

  it should "return 1 for an empty list" in {
    val l = datastructures.List[Double]()
    datastructures.List.product(l) should be (1D)
  }

  "lengthViaFoldLeft" should "produce the length of a list" in {
    val l = datastructures.List(1,2,3,4,5)
    datastructures.List.lengthViaFoldLeft(l) should be (5)
  }

  it should "produce a length of zero for an empty list" in {
    val l = datastructures.List[String]()
    datastructures.List.lengthViaFoldLeft(l) should be (0)
  }

  "reverse" should "reverse a list" in {
    val l = datastructures.List(1,2,3,4,5)
    l.reverse should be (datastructures.List(5,4,3,2,1))
  }

  "reverseViaFoldRight" should "reverse a list" in {
    val l = datastructures.List(1,2,3,4,5)
    datastructures.List.reverseViaFoldRight(l) should be (datastructures.List(5,4,3,2,1))
  }

  "foldLeftViaFoldRight" should "implement the same function as foldLeft" in {
    val l = datastructures.List(1,2,3,4,5)
    foldLeftViaFoldRight(l)(0)(_ + _) should be ( foldLeft(l)(0)(_ + _) )
  }

  "foldRightViaFoldLeft" should "implement create the same function as foldRight" in {
    val l = datastructures.List(1,2,3,4,5)
    foldRightViaFoldLeft(l)(0)(_ + _) should be (foldRight(l)(0)(_ + _))
  }

  "appendViaFoldLeft" should "concatenate two lists" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List(4, 5)
    appendViaFoldLeft(l1, l2) should be (datastructures.List(1,2,3,4,5))
  }

  it should "apply from the List" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List(4, 5)
    l1.append(l2) should be (datastructures.List(1,2,3,4,5))
  }

  "concatenateToSingleList" should "combine a list of lists to a single list" in {
    val l1 = datastructures.List(1, 2)
    val l2 = datastructures.List(3, 4)
    val l3 = datastructures.List(5, 6)
    val l4 = datastructures.List(7, 8)
    val l5 = datastructures.List(9, 10)
    val l = datastructures.List(l1, l2, l3, l4, l5)
    concatenateToSingleList(l) should be (datastructures.List(1,2,3,4,5,6,7,8,9,10))
  }

  it should "remove Nil lists if found" in {
    val l1 = datastructures.List(1,2)
    val l2 = datastructures.List[Int]()
    val l3 = datastructures.List(3,4)
    val l4 = datastructures.List[Int]()
    val l5 = datastructures.List(5)
    val l = datastructures.List(l1, l2, l3, l4 ,l5)
    concatenateToSingleList(l) should be (datastructures.List(1,2,3,4,5))
  }

  "plus1" should "add 1 to each element in a list" in {
    val l = datastructures.List(1,2,3,4,5)
    plus1(l) should be (datastructures.List(2,3,4,5,6))
  }

  "doubleListToString" should "convert a list of double to a list of strings" in {
    val l = datastructures.List(1.0, 2.0, 3.0)
    doubleListToString(l) should be (datastructures.List("1.0", "2.0", "3.0"))
  }

  "map" should "be able to add 1 to each element in a list of int" in {
    val l = datastructures.List(1,2,3,4,5)
    val f: (Int => Int) = _ + 1
    l.map(f) should be (datastructures.List(2,3,4,5,6))
  }

  it should "be able to convert a list of double to a list of strings" in {
    val l = datastructures.List(1.0, 2.0, 3.0)
    val f: (Double => String) = _.toString
    map(l)(f) should be (datastructures.List("1.0", "2.0", "3.0"))
  }

  "filter" should "remove elements that do not fit a predicate" in {
    val l = datastructures.List(1,2,3,4,5)
    val f: (Int => Boolean) = _ >= 3
    filter(l)(f) should be (datastructures.List(3, 4, 5))
  }

  "flatMap" should "take a function with creates lists from the type of the list to create a single list" in {
    val l = datastructures.List(1,2,3)
    val f : (Int => List[Int]) = (i: Int) => datastructures.List(i, i)
    flatMap(l)(f) should be (datastructures.List(1,1,2,2,3,3))
  }

  it should "do the same with the list" in {
    val l = datastructures.List(1,2,3)
    val f : (Int => List[Int]) = (i: Int) => datastructures.List(i, i)
    l.flatMap(f) should be (datastructures.List(1,1,2,2,3,3))
  }

  "filterViaFlatMap" should "remove elements that do not fit a predicate" in {
    val l = datastructures.List(1,2,3,4,5)
    val f: (Int => Boolean) = _ >= 3
    filterViaFlatMap(l)(f) should be (datastructures.List(3,4,5))
  }

  it should "remove elements that do not fit a predicate from the list" in {
    val l = datastructures.List(1,2,3,4,5)
    val f: (Int => Boolean) = _ >= 3
    l.filter(f) should be (datastructures.List(3, 4, 5))
  }

  "addCorrespondingElements" should "add together values of two lists of integers" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List(4,5,6)
    addCorrespondingElements(l1, l2) should be (datastructures.List(5, 7, 9))
  }

  it should "only zip to the length of the shorter of the two lists" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val l2 = datastructures.List(4,5,6)
    addCorrespondingElements(l1, l2) should be (datastructures.List(5,7,9))
  }

  "zip" should "return a tuple of the elements of two lists" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List("I", "am", "awesome!")
    zip(l1, l2) should be (datastructures.List((1, "I"), (2, "am"), (3, "awesome!")))
  }

  it should "only zip to the length of the shorter of the two lists" in {
    val l1 = datastructures.List(1,2,3,4,5,6,7)
    val l2 = datastructures.List("I", "am", "awesome!")
    l1.zip(l2) should be (datastructures.List((1, "I"), (2, "am"), (3, "awesome!")))
  }

  "zipWith" should "be able to add all elements in lists of integers" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List(4,5,6)
    val f : (Int, Int) => Int = _ + _
    zipWith(l1, l2)(f) should be (datastructures.List(5, 7, 9))
  }

  it should "be able to generate new types from the combination" in {
    val l1 = datastructures.List(1.0, 2.0, 3.0)
    val l2 = datastructures.List(4, 5, 6)
    val f : (Double, Int) => String = (d: Double, i: Int) =>  s"${(d.toInt * i).toString}!"
    l1.zipWith(l2)(f) should be (datastructures.List("4!", "10!", "18!"))
  }

  "take" should "only take the number of elements indicated from the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    take(l1)(3) should be (datastructures.List(1,2,3))
  }

  it should "return Nil if the list is Nil" in {
    val l1 = datastructures.List[Int]()
    l1.take(3) should be (datastructures.List[Int]())
  }

  it should "return the entire list if n is larger than the length of the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    take(l1)(12) should be (datastructures.List(1,2,3,4,5))
  }

  "takeWhile" should "return the first elements that pass the test" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ <= 3
    l1.takeWhile(p) should be (datastructures.List(1,2,3))
  }

  it should "return a Nil List if the first element does not pass the predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ > 5
    takeWhile(l1)(p) should be (datastructures.List[Int]())
  }

  it should "return the original list if they all satisfy the predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ < 10
    l1.takeWhile(p) should be (datastructures.List(1,2,3,4,5))
  }

  "forall" should "return true if all elements match a predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ <= 6

    forall(l1)(p) should be (true)
  }

  it should "fail if a single element does not match" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ < 5

    l1.forall(p) should be (false)
  }

  "exists" should "return true if a single element matches the predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p : (Int => Boolean) = _ == 4
    exists(l1)(p) should be (true)
  }

  it should "return false if none of the elements satisfies the predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p: (Int => Boolean) = _ == 27

    l1.exists(p) should be (false)
  }

  it should "return true if they all match the predicate" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val p: (Int => Boolean) = _ < 10
    exists(l1)(p) should be (true)
  }

  "scanRight" should "return successive pieces of the iteration" in {
    val l1 = datastructures.List(1,2,3)
    scanRight(l1)(0)(_ + _) should be (datastructures.List(6,5,3,0))
  }

  it should "return successive pieces of the iteration from the list" in {
    val l1 = datastructures.List(1,2,3)
    l1.scanRight(0)(_ + _) should be (datastructures.List(6,5,3,0))
  }

  "scanLeft" should "return small pieces of the iteration" in {
    val l1 = datastructures.List(1,2,3,4,5)

    scanLeft(l1)(0)(_ + _) should be (datastructures.List(0, 1, 3, 6, 10, 15))
  }

  it should "return small pieces of the iteration from the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    l1.scanLeft(0)(_ + _) should be (datastructures.List(0, 1, 3, 6, 10, 15))
  }

  "splitRight" should "return each Cons as its own list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    splitRight(l1) should be (datastructures.List(datastructures.List(1,2,3,4,5), datastructures.List(2,3,4,5), datastructures.List(3,4,5), datastructures.List(4,5), datastructures.List(5)))
  }

  it should "return each Cons as its own list from the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    l1.splitRight should be (datastructures.List(datastructures.List(1,2,3,4,5), datastructures.List(2,3,4,5), datastructures.List(3,4,5), datastructures.List(4,5), datastructures.List(5)))
  }

  "splitLeft" should "return a list of each list starting from the left" in {
    val l1 = datastructures.List(1,2,3,4,5)
    splitLeft(l1) should be (datastructures.List(datastructures.List(1), datastructures.List(1,2), datastructures.List(1,2,3), datastructures.List(1,2,3,4), datastructures.List(1,2,3,4,5)))
  }

  it should "return a list of each list starting from the left from the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    l1.splitLeft should be (datastructures.List(datastructures.List(1), datastructures.List(1,2), datastructures.List(1,2,3), datastructures.List(1,2,3,4), datastructures.List(1,2,3,4,5)))
  }

  "hasSubsequence" should "return whether a given subsequence is present in the list" in {
    val l1 = datastructures.List(1,2,3,4,5)
    val l2 = datastructures.List(3,4)
    hasSubsequence(l1)(l2) should be (true)
  }

  "::" should "add a new element to a list" in {
    val l = datastructures.List(1, 2, 3)
    1 :: l should be (datastructures.List(1, 1,2,3))
  }

  ":::" should "append two lists together" in {
    val l1 = datastructures.List(1,2,3)
    val l2 = datastructures.List(4,5,6)
    l1 ::: l2 should be (datastructures.List(1,2,3,4,5,6))
  }

}
