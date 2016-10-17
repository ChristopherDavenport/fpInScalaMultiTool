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

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = count match {
      case positive if positive > 0 =>
        val (i, r) = rng.nextInt
        loop(count-1, r, i::acc)
      case _ => (acc, rng)
    }
    loop(count, rng, List[Int]())
  }

  /**
    * Randomly Generated A. State Action where the program depends on some RNG,
    * uses it to generate an A, and transitions to RNG to a new state that
    * can be used later by another action
    * @tparam A Any
    */
  type Rand[+A] = RNG => (A, RNG)


  /**
    * Unit passes the rng along withouth using it always returning a constant
    * value rather than a random value
    * @param a The constant to pass
    * @tparam A The type of the constant
    * @return a new Rand of the type
    */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /**
    * Map does not modify the state of the rng but does modify the given A that is
    * contained within the RNG. Simply that we transform the variable without transforming
    * the RNG state.
    *
    * Important to not that s(rng) is merely applying the given rng to get the result type.
    * @param s The given Rand
    * @param f The function to transform the internal type
    * @tparam A The starting type of the Rand
    * @tparam B The finishing type of the Rand
    * @return A new Rand with the internal value transformed but not
    *         the state of the RNG
    */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /**
    * Implementation gives us  a non Negative Even Random number
    * @return A Rand which only needs a RNG applied to get the desired value type
    */
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /**
    * Exercise 6.5
    * This simple Maps Over the set of nonNegativeInts and creates a double of the value
    * between zero and 1 based on its distance between the value to double and the max int value
    * @return a Rand of Dobule
    */
  def _double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  // Exercise 6.6
  /**
    * Exercise 6.6 -
    * This function combines two actions and combines their result into another random
    * action that combines the two
    * @param ra The first action of type A
    * @param rb The second actio if type B
    * @param f The combination function
    * @tparam A Type of First Action
    * @tparam B Type of Second Action
    * @tparam C Result Type
    * @return A function of Rand C
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  /**
    * Pair to combine
    * @param ra First Action
    * @param rb Second Action
    * @tparam A Type of first Action
    * @tparam B Type of Second Action
    * @return Tuple of Action results
    */
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
    * Exercise 6.7
    * Since this now operates as monoid in that it is a semigroup with a zero value.
    * The zero in an RNG of an empty list and we can foldLeft over the list by mapping
    * the generated values and applying them in sequential order through each element of
    * the list or doing nothing. So we build the Lieft from a list of A passed with the rng
    * to the next value and then continue passing the newly generated value as we build the whole
    * list, then as we have build the list in reverse we reverse it to get the original list
    * expected
    * @param fs The list of Rands
    * @tparam A the type of Randoms generated
    * @return A Rand of List of A
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val a = fs.foldLeft(RNG.unit(List[A]()))((acc, f) => _map2(f, acc)(_ :: _))
    _map(a)(_.reverse)
  }

  def _int : Rand[Int] = rng => rng.nextInt

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(_int))
  }

  /**
    * Exercise 6.8
    *
    * This one is tricky because it is supplying extra parameters for the required function.
    * First we deconstruct the rand application to generate the new state and the new rand, and
    * then we must must the input to the function and the rand into as the second parameter for
    * the type as it takes and RNG => Rand so by supplying the extra parameter we still meet those
    * criteria
    *
    * @param f The original Rand
    * @param g A Function which takes A to Rand[B]
    * @tparam A The Type of the Original Rand
    * @tparam B The type of the Returned Rand
    * @return A Rand of Type B transformed by the randomness
    */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
        val mod = i % n
        if ( i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /**
    * Exercise 6.9 -
    * Map Is quite simple because we take the internal value and use unit to lift the function back into the Rand
    *
    * @param s The Rand To Lift
    * @param f The function to transform within the Rand
    * @tparam A The origin Type of Rand
    * @tparam B The final type of Raand
    * @return A Rand of B
    */
  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s){ b => unit(f(b))}

  /**
    * Exercise 6.9 -
    * Map2 Uses Map and Flatmap which allows us to use a for comprehension rather than flatmap, then the call
    * to map. Then the final function is called on the two intermediate products as though they were nested.
    *
    * @param ra The first rand
    * @param rb The second Rand
    * @param f The Function that transforms the internals of the two rands
    * @tparam A The type of the first rand
    * @tparam B The type of the second rand
    * @tparam C The type of the return Rand
    * @return A Rand of Type C
    */
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra){a => _map(rb){ b => f(a,b)}}

  /**
    * Generates a random number between 1 and 6
    * @return A random number between 1 and 6
    */
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}


