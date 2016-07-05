package io.christopherdavenport.fpinscalamultitool.Vanilla

import io.christopherdavenport.fpinscalamultitool.Vanilla.Chapter2._
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 7/2/16.
  */
class Chapter2Tests extends FlatSpec with Matchers{

  "fib" should "return 0 for its first member" in {
    assert(fib(1) === 0)
  }

  it should "return 1 for its second number" in {
    assert(fib(2) === 1)
  }

  it should "return the sequence appropriately for a range" in {
    val range = 1 to 25
    val result = range.map(fib)
    assert(
      result === Vector(
        0, 1, 1, 2, 3, 5, 8,
        13, 21, 34, 55, 89, 144,
        233, 377, 610, 987, 1597,
        2584, 4181, 6765, 10946,
        17711, 28657, 46368
      )
    )
  }

  "isSorted" should "succesfully tell whether an array is sorted" in {
    val as = Array(1, 2, 3)
    val f : (Int, Int) => Boolean = _ < _

    assert(isSorted(as, f) === true )

  }

  it should "successfull tell if ordered with the last element incorrectly sorted" in {
    val as = Array(1, 2, 1)
    val f : (Int, Int) => Boolean = _ < _

    assert(isSorted(as, f) === false )
  }

  it should "successfull tell if ordered with the first element incorrectly sorted" in {
    val as = Array(5, 2, 3)
    val f : (Int, Int) => Boolean = _ < _

    assert(isSorted(as, f) === false )
  }

  "curry" should "change a 2 parameter function to a single parameter function taking a second value to complete" in {
    val f: (Int, Int) => Int = (a: Int, b: Int) => a + b

    val fCurried = curry(f)

    assert(fCurried(5)(7) === f(5, 7))
  }

  "uncurry" should "change a curried 2 parameter function to a single 2 parameter function" in {
    val f: (Int) => (Int) => Int = (a: Int) => (b: Int) => a + b
    val fUncurried = uncurry(f)

    assert(f(5)(7) === fUncurried(5, 7))
  }

}
