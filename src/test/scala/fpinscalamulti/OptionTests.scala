package fpinscalamulti

import org.scalatest.{FlatSpec, Matchers}
import fpinscalamulti.{Option => option}
import fpinscalamulti.{Some => some}
import fpinscalamulti.{None => none}

/**
  * Created by davenpcm on 7/5/16.
  */
class OptionTests extends FlatSpec with Matchers{

  "apply" should "create a some if there is a value" in {
    val i = 1
    Option(i) should be (some(1))
  }

  it should "create a none with a null" in {
    val i = null
    Option(i) should be (None)
  }

  "map" should "return an option changed by a function" in {
    val f: Int => Int = _ + 1
    val o = some(1)
    o.map(f) should be (some(2))
  }

  "getOrElse" should "return the value inside a function if it exists" in {
    val o = some(1)
    o.getOrElse(2) should be (1)
  }

  it should "return the getOrElse value if it is None" in {
    val o : option[Int] = none
    o.getOrElse(2) should be (2)
  }

  "flatMap" should "transform a Some of 1 type to another" in {
    val o = some(1)
    val f: Int => option[Int] = (i: Int) => some(i + 1)
    o.flatMap(f) should be (some(2))
  }

  it should "leave Nones as Nones" in {
    val o : option[Int] = None
    val f: Int => option[Int] = (i: Int) => some(i + 1)
    o.flatMap(f) should be (none)
  }

  it should "flatten nested options" in {
    val o = some(some(1))
    o.flatMap(identity) should be (some(1))
  }

  "orElse" should "return the Option when it is Some" in {
    val o = some(1)
    o.orElse(some(2)) should be (some(1))
  }

  it should "return the second value if it is none" in {
    val o : option[Int] = none
    o.orElse(some(2)) should be (some(2))
  }

  "filter" should "leave the Some if it matches the predicate" in {
    val o = some(1)
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (None)
  }

  it should "filter it to None if it does not match the predicate" in {
    val o = some(2)
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (some(2))
  }

  it should "return None if it is already None" in {
    val o : option[Int] = None
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (None)
  }

  "lift" should "elevate a function to work on options" in {
    val f : Int => Int = _ + 1
    val o = some(2)
    option.lift(f)(o) should be (some(3))
  }

  "map2" should "return the option returned from the combination of 2 options" in {
    val f : (Int, Int) => Int = _ + _
    val o1 = some(2)
    val o2 = some(5)
    option.map2(o1, o2)(f) should be (some(7))
  }

}
