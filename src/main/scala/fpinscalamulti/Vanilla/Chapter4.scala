package fpinscalamulti.Vanilla

import fpinscalamulti.Some
import fpinscalamulti.Option
import fpinscalamulti.None
/**
  * Created by davenpcm on 7/4/16.
  */
object Chapter4 {

  /**
    * Exercise 4.2
    * The Only point of failure is if the sequence is empty. Checking for that makes all future calculations easy and
    * safe. Need to see what was meant through implementing with flatmap as taht would be for the elements. I dont
    * see a need here.
    * @param xs The sequence of dobules
    * @return An Option of a double of the variance if the sequence is longer than 0
    */
  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val m = xs.sum/xs.length
      val variance = xs.foldRight(0D)((x, agg) =>  Math.pow(x - m, 2) + agg )/xs.length
      Some(variance)
    }

}
