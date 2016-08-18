package fpinscalamulti.Vanilla.errorhandling

import Chapter4._
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 7/23/16.
  */
class Chapter4Tests extends FlatSpec with Matchers{
  "Variance" should "return the correct variance for a Sequence of Doubles" in {
    val xs = List(1D,2D,2D,4D,4D,5D)
    variance(xs) should be (Some(2.0))
  }

  it should "return None on an empty Sequence" in {
    val xs = List()
    variance(xs) should be (None)
  }

}
