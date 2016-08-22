package fpinscalamulti.Vanilla.state

/**
  * Created by davenpcm on 8/21/16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  /**
    * Exercise 6.1
    * We Bump The integer up by one on the negative side to deal with int min value
    * @param rng The RNG
    * @return An Int and an RNG
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    i match {
      case lessThanZero if lessThanZero < 0 => ( -(lessThanZero + 1), rng2)
      case _ => (i, rng2)
    }
  }

  /**
    * Exercise 6.2
    * Generates a double between zero and 1
    * @param rng The rng
    * @return
    */
  def double(rng: RNG): (Double, RNG) = {
    val noNeg = nonNegativeInt(rng)
    (noNeg._1.toDouble / Int.MaxValue.toDouble, noNeg._2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG):((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i,d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = count match {
      case positive if positive > 0 =>
        val (i, r) = rng.nextInt
        loop(count-1, r, i::acc)
      case _ => (acc, rng)
    }
    loop(count, rng, List[Int]())
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def _double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val a = fs.foldLeft(RNG.unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))
    map(a)(_.reverse)
  }


}


