package fpinscalamulti.Vanilla.laziness

import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 7/24/16.
  */
class StreamTests extends FlatSpec with Matchers {

  "Apply" should "build an Empty Stream" in {
    val s = Stream()
    s should be (Empty)
  }

  it should "build a stream out of elements" in {
    val s = Stream(1)
    s.headOption should be (Some(1))
  }

  "Empty" should "be an Empty Stream" in {
    val s = Empty
    s should be (Empty)
  }

  "Cons" should "build a Stream" in {
    val s = Cons(() => 1,() => Cons( () => 2, () => Cons(() => 3, () => Empty)))
    s.toList should be (List(1,2,3))
  }

  "cons" should "build a stream without thunks" in {
    val s = Stream.cons(1, Stream.cons(2, Stream.cons(3,Empty)))
    s.toList should be (List(1,2,3))
  }

  "empty" should "create an empty Stream" in {
    val s = Stream.empty
    s should be (Empty)
  }

  "headOption" should "return None on an Empty Stream" in {
    val s = Stream()
    s.headOption should be (None)
  }

  it should "return some if the stream has a first element" in {
    val s = Stream(1,2,3,4,5)
    s.headOption should be (Some(1))
  }

  "toList" should "force evaluation of all elements" in {
    val s = Stream(1,2,3)
    s.toList should be (List(1,2,3))
  }

  it should "build an empty list if the Stream is empty" in {
    val s = Stream()
    s.toList should be (Nil)
  }

  "drop" should "drop elements from a stream without evaluation" in {
    // We force evaluation for comparison with toList
    val s = Stream(1,2,3)
    s.drop(1).toList should be (List(2,3))
  }

  it should "return empty if it is asked to drop more than all the elements in the stream" in {
    val s = Stream(1,2,3)
    s.drop(5) should be (Empty)
  }

  "take" should "return a stream with that many elements taken if it has enough" in {
    val s = Stream(1,2,3)
    s.take(2).toList should be (List(1,2))
  }

  it should "return empty on an empty list" in {
    val s = Stream()
    s.take(5) should be (Empty)
  }

  it should "return the original stream if asked to take more elements than the original stream" in {
    val s = Stream(1,2,3)
    s.take(5).toList should be (s.toList)
  }

  "takeWhile1" should "take while a condition is met" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhile1(_ < 3).toList should be (List(1,2))
  }

  it should "return empty if the first does not meet the criteria" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhile1(_ > 1) should be (Empty)
  }

  "exists" should "return true if a value in the stream matches the predicate" in {
    val s = Stream(1,2,3,4,5)
    s.exists(_ == 1) should be (true)
  }

  it should "return false if no values match the predicate" in {
    val s = Stream(1,2,3,4,5)
    s.exists(_ == 100) should be (false)
  }

  "forall" should "return true if all values of the stream match the predicate" in {
    val s = Stream(1,2,3,4,5)
    s.forAll(_ < 10) should be (true)
  }

  it should "return false if any value fails to match the predicate" in {
    val s = Stream(1,2,3,4,5)
    s.forAll(_ < 5) should be (false)
    s.forAll(_ == 1) should be (false)
  }

  "takeWhile" should "take while a condition is met" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhile(_ < 3).toList should be (List(1,2))
  }

  it should "return empty if the first does not meet the criteria" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhile(_ > 1) should be (Empty)
  }

  "headOptionFoldR" should "return None on an Empty Stream" in {
    val s = Stream()
    s.headOptionFoldR should be (None)
  }

  it should "return some if the stream has a first element" in {
    val s = Stream(1,2,3,4,5)
    s.headOptionFoldR should be (Some(1))
  }

  "isEmpty" should "return true when a stream is empty" in {
    val s = Stream()
    s.isEmpty should be (true)
  }

  it should "return false if it is not" in {
    val s = Stream(1,2,3,4)
    s.isEmpty should be (false)
  }

  "isDefined" should "return false when a stream is empty" in {
    val s = Stream()
    s.isDefined should be (false)
  }

  it should "return true if the stream has elements" in {
    val s = Stream(1,2,3,4)
    s.isDefined should be (true)
  }

  "map" should "transform the values of a stream" in {
    val s = Stream(1,2,3,4,5)
    s.map(_ + 5).toList should be (List(6,7,8,9,10))
  }

  it should "Only change an empty to a new type" in {
    val s : Stream[Int] = Stream.empty
    s.map(_.toDouble) should be (Empty: Stream[Double])
  }

  "filter" should "filter values that do not match a predicate" in {
    val s = Stream(1,2,3,4,5,6)
    s.filter(_ % 2 == 0).toList should be (List(2,4,6))
  }

  "append" should "concatenate two streams" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(4,5,6)
    s1.append(s2).toList should be (List(1,2,3,4,5,6))
  }

  it should "do nothing if an empty stream is appended" in {
    val s = Stream(1,2,3)
    s.append(Empty: Stream[Int]).toList should be (s.toList)
  }

  "flatMap" should "take a function that forms a stream and create a new stream" in {
    val s = Stream(1,2,3)
    val f : Int => Stream[Int] = i => Stream(i, i*2, i*3)
    s.flatMap(f).toList should be (List(1,2,3,2,4,6,3,6,9))
  }

  "find" should "find the first element that matches a predicate" in {
    val s = Stream(1,2,3)
    s.find(_ % 2 == 0) should be (Some(2))
  }

  it should "return None if no element matches it" in {
    val s = Stream(1,2,3)
    s.find(_ == 10) should be (None)
  }

  "constant" should "return an infinite stream" in {
    val s = Stream.constant(1)
    s.take(5).toList should be (List(1,1,1,1,1))
  }

  "from" should "return an ever increasing stream" in {
    val s = Stream.from(5)
    s.filter(_ % 2 == 0).take(5).toList should be (List(6,8,10,12,14))
  }

  "fibs" should "return the fibonacci sequence" in {
    Stream.fibs.take(7).toList should be (List(0,1,1,2,3,5,8))
  }

  "fibsUnfold" should "return the fibonacci sequence" in {
    Stream.fibsUnfold.take(7).toList should be (List(0,1,1,2,3,5,8))
  }

  "fromUnfold" should "return an ever increasing stream" in {
    val s = Stream.fromUnfold(5)
    s.filter(_ % 2 == 0).take(5).toList should be (List(6,8,10,12,14))
  }

  "constantUnfold" should "return an infinite stream" in {
    val s = Stream.constantUnfold(1)
    s.take(5).toList should be (List(1,1,1,1,1))
  }
  "mapUnfold" should "transform the values of a stream" in {
    val s = Stream(1,2,3,4,5)
    s.mapUnfold(_ + 5).toList should be (List(6,7,8,9,10))
  }

  it should "Only change an empty to a new type" in {
    val s : Stream[Int] = Stream.empty
    s.mapUnfold(_.toDouble) should be (Empty: Stream[Double])
  }

  "takeUnfold" should "return a stream with that many elements taken if it has enough" in {
    val s = Stream(1,2,3)
    s.takeUnfold(2).toList should be (List(1,2))
  }

  it should "return empty on an empty list" in {
    val s = Stream()
    s.takeUnfold(5) should be (Empty)
  }

  it should "return the original stream if asked to take more elements than the original stream" in {
    val s = Stream(1,2,3)
    s.takeUnfold(5).toList should be (s.toList)
  }

  "takeWhileUnfold" should "take while a condition is met" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhileUnfold(_ < 3).toList should be (List(1,2))
  }

  it should "return empty if the first does not meet the criteria" in {
    val s = Stream(1,2,3,4,5)
    s.takeWhileUnfold(_ > 1) should be (Empty)
  }

  "zipWith" should "zip 2 Streams together" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(5,6,7)
    s1.zipWith(s2).toList should be (List((1,5),(2,6),(3,7)))
  }

  it should "terminate when either list runs out of values" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(5,6)
    s1.zipWith(s2).toList should be (List((1,5),(2,6)))
  }

  it should "return empty if either is empty" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream.empty[Int]
    s1.zipWith(s2) should be (s2)
  }

  "zipAll" should "zip the entirety of two stream together as options" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(5,6,7)
    val r = List((Some(1),Some(5)), (Some(2), Some(6)), (Some(3), Some(7)))
    s1.zipAll(s2).toList should be (r)
  }

  it should "continue for the entirety of both list with None if the first list is longer" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(5,6)
    val r = List(
      (Some(1), Some(5)),
      (Some(2), Some(6)),
      (Some(3), None)
    )
    s1.zipAll(s2).toList should be (r)
  }

  it should "continue for the entirety of both lists if the first list is empty" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream(5,6)
    val r = List(
      (None, Some(5)),
      (None, Some(6))
    )
    s1.zipAll(s2).toList should be (r)
  }

  "startsWith" should "return true for a shorter second sequence that starts the first sequence" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(1,2)
    s1.startsWith(s2) should be (true)
  }

  it should "return false if the second is longer than the first" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(1,2,3,4,5)
    s1.startsWith(s2) should be (false)
  }

  "tails" should "return the Stream of suffixes of the impute sequence starting with the original stream" in {
    val s = Stream(1,2,3)

    s.tails.mapUnfold(_.toList).toList should be (List(List(1,2,3), List(2,3), List(3), List()))
  }

  "hasSubsequence" should "return true when given a subsequence of the first stream" in {
    val s1 = Stream(1,2,3,4,5)
    val s2 = Stream(3,4)
    s1.hasSubsequence(s2) should be (true)
  }

  it should "return false when it does not contain the subsequence" in {
    val s1 = Stream(1,2,3,4,5)
    val s2 = Stream(1,3,5)
    s1.hasSubsequence(s2) should be (false)
  }

  "scanRight" should "return a stream of intermediate results" in {
    val s = Stream(1,2,3)
    s.scanRight(0)(_ + _).toList should be (List(6,5,3,0))
  }


}
