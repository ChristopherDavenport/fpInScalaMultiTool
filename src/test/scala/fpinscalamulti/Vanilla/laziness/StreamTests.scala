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

}
