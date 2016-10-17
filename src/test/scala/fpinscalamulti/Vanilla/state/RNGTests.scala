package fpinscalamulti.Vanilla.state

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Gen

/**
  * Created by davenpcm on 8/21/16.
  */
class RNGTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
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

  it should "return a positive int for all seeds" in {
    forAll("seed"){ l: Long =>
      val rngInit = RNG.SimpleRNG(l)
      val (int, rng) = RNG.nonNegativeInt(rngInit)
      int should be >= 0
    }
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

  it should "return a value between 0 and 1 forAll seeds" in {
    forAll("seed"){ l: Long =>
      val rngInit = RNG.SimpleRNG(l)
      val (doubleval, rng) = RNG.double(rngInit)

      doubleval should be >= 0D
      doubleval should be <= 1D

    }
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

  /**
    * Basically in unit if applied we should see both givens advanced into the
    * tuple that we can extract.
    */
  "unit" should "return a Rand of the type if given an RNG" in {
    val rngStart = SimpleRNG(42L)
    val (int, rng) = RNG.unit(1)(rngStart)
    int should be (1)
    rng should be (rngStart)
  }

  it should "elevate both values to the RNG context forAll values" in {
    forAll("value", "seed"){ (i: Int, l: Long) =>
      val rngStart = SimpleRNG(l)
      val (int, rng) = RNG.unit(i)(rngStart)
      int should be (i)
      rng should be (rngStart)
    }
  }

  "map" should "transform the internal function without transforming the Rand forAll ints" in {
    forAll("intValue", "seed"){(i: Int, l: Long) =>
      val rngInit = SimpleRNG(l)
      val f : Int => Int = _ % 2
      val a = map(RNG.unit(i))(f)
      val (int, rng) = a(rngInit)
      if (i % 2 == 0) int should be (0)
      else if ( i < 0 ) int should be (-1)
      else int should be (1)
    }
  }

  "nonNegativeEven" should "allow us to get a non negative even int from seeds" in {
    forAll ("seed") { l : Long =>
      val rng = SimpleRNG(l)
      val (int, _) = nonNegativeEven(rng)
      int should be >= 0
      int % 2 should be (0)
    }
  }

  "_double" should "generate a random double between zero and 1 forAll seeds" in {
    forAll("seed"){ l: Long =>
      val rngInit = SimpleRNG(l)
      val (double, _) = _double(rngInit)
      double should be <= 1D
      double should be >= 0D
    }
  }

  "map2" should "combine 2 Rands to create a third based on a combining function" in {
    forAll { (l: Long) =>
      val rngInit = SimpleRNG(l)
      val f : (Int, Int) => Int = (i, j) => i - j

      val (nonNegEven1, rng1) = nonNegativeEven(rngInit)
      val (nonNegEven2, rng2) = nonNegativeEven(rng1)
      val resultInt = f(nonNegEven1, nonNegEven2)

      val (forAllInt, rngFin) = map2(nonNegativeEven, nonNegativeEven)(f)(rngInit)

      forAllInt should be (resultInt)
      rngFin should be (rng2)

    }
  }

  "randIntDouble" should "generate a Random tuple of int and double" in {
    forAll{ (seed: Long ) =>
      val rngInit = SimpleRNG(seed)
      val ((int, double), rngFin) = randIntDouble(rngInit)
      int shouldBe a [java.lang.Integer]
      double shouldBe a [java.lang.Double]
    }
  }

  "randDoubleInt" should "generate a Random tuple of double and int" in {
    forAll{ (seed: Long ) =>
      val rngInit = SimpleRNG(seed)
      val ((double, int), rngFin) = randDoubleInt(rngInit)
      int shouldBe a [java.lang.Integer]
      double shouldBe a [java.lang.Double]
    }
  }

  val positiveReasonableSizedInt = for {
    i <- Gen.choose(0, 1000)
  } yield i

  val arbLong = for {
    l <- Gen.choose(Long.MinValue, Long.MaxValue)
  } yield l

  "_ints" should "generate a random List of Ints of a given size" in {
    forAll(positiveReasonableSizedInt, arbLong ){ (length: Int, seed: Long) =>
      whenever (length >= 0) {
        val rngInt = SimpleRNG(seed)

        val (list, rngFin) = _ints(length)(rngInt)
        list.length should be (length)
        list.forall(_ <= Int.MaxValue) should be (true)
      }

    }
  }

  "nonNegativeLessThan" should "generate a random nonNegativeInt Less Than a number" in {
    forAll("lessThan", "seed") { (lessThan: Int, seed: Long) =>
      whenever(lessThan > 0) {
        val rngInit = SimpleRNG(seed)
        val (int, _) = nonNegativeLessThan(lessThan)(rngInit)
        int should be < lessThan
        int should be >= 0
      }
    }
  }

  "_map" should "transform the internal function without transforming the Rand forAll ints" in {
    forAll("intValue", "seed"){(i: Int, l: Long) =>
      val rngInit = SimpleRNG(l)
      val f : Int => Int = _ % 2
      val a = _map(RNG.unit(i))(f)
      val (int, rng) = a(rngInit)
      if (i % 2 == 0) int should be (0)
      else if ( i < 0 ) int should be (-1)
      else int should be (1)
    }
  }

  "_map2" should "combine 2 Rands to create a third based on a combining function" in {
    forAll("seed") { (l: Long) =>
      val rngInit = SimpleRNG(l)
      val f : (Int, Int) => Int = (i, j) => i - j

      val (nonNegEven1, rng1) = nonNegativeEven(rngInit)
      val (nonNegEven2, rng2) = nonNegativeEven(rng1)
      val resultInt = f(nonNegEven1, nonNegEven2)

      val (forAllInt, rngFin) = _map2(nonNegativeEven, nonNegativeEven)(f)(rngInit)

      forAllInt should be (resultInt)
      rngFin should be (rng2)

    }
  }

  "rollDie" should "produce an Int between 1 and 6" in {
    forAll("seed"){ seed: Long =>
      val (int, _) = rollDie(SimpleRNG(seed))
      int should be >= 1
      int should be <= 6
    }
  }




}
