package fpinscalamulti.Vanilla

import org.scalatest.{FlatSpec, Matchers}
import fpinscalamulti.Option
import fpinscalamulti.Some
import fpinscalamulti.None
/**
  * Created by davenpcm on 7/23/16.
  */
class Chapter4Tests extends FlatSpec with Matchers{
  "Variance" should "return the correct variance for a Sequence of Doubles" in {
    val xs = List(1D,2D,2D,4D,4D,5D)
    Chapter4.variance(xs) should be (Some(2.0))
  }

  it should "return None on an empty Sequence" in {
    val xs = List()
    Chapter4.variance(xs) should be (None)
  }

}
