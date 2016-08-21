package fpinscalamulti.Vanilla.errorhandling

import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 7/24/16.
  */
class EitherTests extends FlatSpec with Matchers{

  "isRight" should "return true if the value is right" in {
    val e = Right("Value Here")
    e.isRight should be (true)
  }

  it should "return false if the value is left" in {
    val e = Left(new Exception("Coolio"))
    e.isRight should be (false)
  }

  "isLeft" should "return true if the value is left" in {
    val e = Left(new Exception("Coolio"))
    e.isLeft should be (true)
  }

  it should "return false if the value is right" in {
    val e = Right("Value Here")
    e.isLeft should be (false)
  }


  "Try" should "return Right if No exception is thrown" in {
    val s = "1"
    Either.Try(s.toInt) should be (Right(1))
  }

  it should "return Left if an exception is thrown" in {
    val s = "yellow"
    lazy val shared = new Exception("TestException")
    val fError : String => Int = _ => throw shared
    Either.Try(fError(s)) should be (Left(shared))
  }

  "map" should "take a function over the right side of an either and move that forward" in {
    val s = "1"
    val f : Int => Int = _ + 1
    Either.Try(s.toInt).map(f) should be (Right(2))
  }

  it should "return the left if it was already left" in {
    val s = "yellow"
    val f : Int => Int = _ + 1
    lazy val shared = new Exception("TestException")
    val fError : String => Int = _ => throw shared
    Either.Try(fError(s)).map(f) should be (Left(shared))
  }

  "flatMap" should "take an  Either and return the function applied to the right" in {
    val s = "1"
    val e = Either.Try(s.toInt)
    val f : Int => Either[String, Int] = i => if (i == 0) Left("Can't Divide By 0") else Right(i / i)
    e.flatMap(f) should be (Right(1))
  }

  it should "return the Original Left if it was originally left" in {
    val e: Either[String, Int] = Left("Cant take mean of empty list!")
    val f : Int => Either[String, Int] = i => if (i == 0) Left("Can't Divide By 0") else Right(i / i)
    e.flatMap(f) should be (Left("Cant take mean of empty list!"))
  }

  "orElse" should "return the Either if it is right" in {
    val s = "1"
    val e = Either.Try(s.toInt)
    e.orElse(Right(-1)) should be (Right(1))
  }

  it should "return the Other Either if it is left" in {
    val s = "yellow"
    val e = Either.Try(s.toInt)
    e.orElse(Right(-1)) should be (Right(-1))
  }

  "map2" should "take two Eithers and combine them to a new Either" in {
    val e1 = Right(1)
    val e2 = Right(2)
    val f : (Int, Int) => Int = _ + _
    e1.map2(e2)(f) should be (Right(3))
  }

  it should "return the first left if only it is Left" in {
    val e1: Either[String, Int] = Left("Bad")
    val e2: Either[String, Int] = Right(2)
    val f : (Int, Int) => Int = _ + _
    e1.map2(e2)(f) should be (Left("Bad"))
  }

  it should "return the second left if only it is left" in {
    val e1: Either[String, Int] = Right(1)
    val e2: Either[String, Int] = Left("Bad")
    val f : (Int, Int) => Int = _ + _
    e1.map2(e2)(f) should be (Left("Bad"))
  }

  it should "return the first left if both are left" in {
    val e1: Either[String, Int] = Left("Bad")
    val e2: Either[String, Int] = Left("Bad 2 - Don't Get Me!")
    val f : (Int, Int) => Int = _ + _
    e1.map2(e2)(f) should be (Left("Bad"))
  }

  "traverse" should "return a right of a list from a function that goes to an either" in {
    val l = List("1", "2", "3")
    val f : String => Either[Exception, Int] = s => Either.Try(s.toInt)
    Either.traverse(l)(f) should be (Right(List(1,2,3)))
  }

  it should "return the left if any go to left" in {
    val l = List(1,2,3,4)
    val f : Int => Either[String, Int] = i => if (i==2) Left("No Twos") else Right(i)
    Either.traverse(l)(f) should be (Left("No Twos"))
  }

  "sequence" should "return the List if all are Right" in {
    val l = List(Right(1), Right(2), Right(3))
    Either.sequence(l) should be (Right(List(1,2,3)))
  }

  it should "return the left if any is left" in {
    val l = List(Right(1), Left("Monkey"), Right(3))
    Either.sequence(l) should be (Left("Monkey"))
  }

}
