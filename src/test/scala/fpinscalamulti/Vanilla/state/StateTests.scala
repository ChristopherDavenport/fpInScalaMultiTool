package fpinscalamulti.Vanilla.state

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by davenpcm on 8/21/16.
  */
class StateTests extends FlatSpec with Matchers {
  import RNG._

  "Simple RNG"  should "return integers based on seed" in {
    val rng = RNG.SimpleRNG(42L)

    rng.nextInt should be ((16159453, RNG.SimpleRNG(1059025964525L)))
    rng.nextInt should be ((16159453, RNG.SimpleRNG(1059025964525L)))
  }

  it should "return in an expected order" in {
    val rng = RNG.SimpleRNG(42L)
    val rng2 = rng.nextInt._2
    rng2.nextInt should be ((-1281479697, RNG.SimpleRNG(197491923327988L)))
  }

  "nonNegativeInt" should "return an expected positive int for a given RNG" in {
    val rng = RNG.SimpleRNG(103L)
    RNG.nonNegativeInt(rng) should be ((39629136, RNG.SimpleRNG(2597135103462L)))
  }

  it should "return a postivie int for a seed value nextInt will be negative" in {
    val rng = RNG.SimpleRNG(107785L)
    rng.nextInt should be ((-1479512763,RNG.SimpleRNG(184513628297952L)))
    RNG.nonNegativeInt(rng) should be ((1479512762,RNG.SimpleRNG(184513628297952L)))
  }

  "double" should "return an expected value between 0 and 1" in {
    val rng = RNG.SimpleRNG(42L)
    RNG.double(rng) should be ((0.007524831689672932,RNG.SimpleRNG(1059025964525L)))
  }

  "intDouble" should "return a specific value for a seed" in {
    val rng = RNG.SimpleRNG(42L)
    RNG.intDouble(rng) should be (((16159453,0.596735485175967),RNG.SimpleRNG(197491923327988L)))
  }

  "doubleInt" should "return a specific value for a seed" in {
    val rng = RNG.SimpleRNG(42L)
    RNG.doubleInt(rng) should be (((0.007524831689672932,-1281479697),SimpleRNG(197491923327988L)))
  }

  "double3" should "return a specific value for a seed" in {
    val rng = RNG.SimpleRNG(42L)
    RNG.double3(rng) should be(
      (0.007524831689672932, 0.596735485175967, 0.15846728401187216),
      SimpleRNG(259172689157871L)
    )
  }

  "ints" should "return a list of ints from a given seed" in {
    val rng = RNG.SimpleRNG(42L)
    ints(5)(rng) should be ((List(1770001318, -2015756020, -340305902, -1281479697, 16159453),
      SimpleRNG(115998806404289L)))
  }


}
