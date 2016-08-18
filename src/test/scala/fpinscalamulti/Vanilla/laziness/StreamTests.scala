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

  "toList" should "force evaluation of all elements" in {
    val s = Stream(1,2,3)
    s.toList should be (List(1,2,3))
  }

  it should "build an empty list if the Stream is empty" in {
    val s = Stream()
    s.toList should be (Nil)
  }

}
