package fpinscalamulti

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by davenpcm on 7/5/16.
  */
class OptionTests extends FlatSpec with Matchers{

  "apply" should "create a Some if there is a value" in {
    val i = 1
    Option(i) should be (Some(1))
  }

  it should "create a None with a null" in {
    val i = null
    Option(i) should be (None)
  }

  "map" should "return an Option changed by a function" in {
    val f: Int => Int = _ + 1
    val o = Some(1)
    o.map(f) should be (Some(2))
  }

  "getOrElse" should "return the value inside a function if it exists" in {
    val o = Some(1)
    o.getOrElse(2) should be (1)
  }

  it should "return the getOrElse value if it is None" in {
    val o : Option[Int] = None
    o.getOrElse(2) should be (2)
  }

  "flatMap" should "transform a Some of 1 type to another" in {
    val o = Some(1)
    val f: Int => Option[Int] = (i: Int) => Some(i + 1)
    o.flatMap(f) should be (Some(2))
  }

  it should "leave Nones as Nones" in {
    val o : Option[Int] = None
    val f: Int => Option[Int] = (i: Int) => Some(i + 1)
    o.flatMap(f) should be (None)
  }

  it should "flatten nested Options" in {
    val o = Some(Some(1))
    o.flatMap(identity) should be (Some(1))
  }

  "orElse" should "return the Option when it is Some" in {
    val o = Some(1)
    o.orElse(Some(2)) should be (Some(1))
  }

  it should "return the second value if it is None" in {
    val o : Option[Int] = None
    o.orElse(Some(2)) should be (Some(2))
  }

  "filter" should "leave the Some if it matches the predicate" in {
    val o = Some(1)
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (None)
  }

  it should "filter it to None if it does not match the predicate" in {
    val o = Some(2)
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (Some(2))
  }

  it should "return None if it is already None" in {
    val o : Option[Int] = None
    val f: Int => Boolean = _ % 2 == 0
    o.filter(f) should be (None)
  }

  "lift" should "elevate a function to work on Options" in {
    val f : Int => Int = _ + 1
    val o = Some(2)
    Option.lift(f)(o) should be (Some(3))
  }

  "map2" should "return the Option returned from the combination of 2 Options" in {
    val f : (Int, Int) => Int = _ + _
    val o1 = Some(2)
    val o2 = Some(5)
    Option.map2(o1, o2)(f) should be (Some(7))
  }

  "isDefined" should "return true if an Option is Some" in {
    val o = Some(2)
    o.isDefined should be (true)
  }

  it should "return false if an Option is None" in {
    val o : Option[Int] = None
    o.isDefined should be (false)
  }

  "isEmpty" should "return true if an Option in None" in {
    val o : Option[Int] = None
    o.isEmpty should be (true)
  }

  it should "return false if an Option is Some" in {
    val o = Some(2)
    o.isEmpty should be (false)
  }
  
  "sequence" should "return Some list if all the Options are defined" in {
    val oList = List(Some(1), Some(2), Some(3), Some(4))
    Option.sequence(oList) should be (Some(List(1,2,3,4)))
  }
  
  it should "return None if one of the Options is not" in {
    val oList = List(Some(1), Some(2), None, Some(4))
    Option.sequence(oList) should be (None)
  }

  it should "return Some empty list if the List is Empty" in {
    val oList = List[Option[Int]]()
    Option.sequence(oList) should be (Some(List[Int]()))
  }

  "Try" should "return some if No exception is thrown" in {
    val i = 1
    Option.Try(i) should be (Some(1))
  }

  it should "return None if an exception is thrown" in {
    val s = "yellow"
    Option.Try(s.toInt) should be (None)
  }

  "traverse" should "take an option that can fail and return some if all pass" in {
    val list = List("1", "2", "3")
    val f : String => Option[Int] = (s) => Option.Try(s.toInt)
    Option.traverse(list)(f) should be (Some(List(1,2,3)))
  }

  it should "return None if any of the values fail to parse" in {
    val list = List("1", "2", "cockadoodledoo", "3")
    val f : String => Option[Int] = (s) => Option.Try(s.toInt)
    Option.traverse(list)(f) should be (None)
  }


}
