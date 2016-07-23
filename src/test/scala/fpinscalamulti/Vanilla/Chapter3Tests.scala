package fpinscalamulti.Vanilla

import fpinscalamulti.List._
import fpinscalamulti.Nil
import fpinscalamulti.Cons
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 7/3/16.
  */
class Chapter3Tests extends FlatSpec with Matchers{

  "Exercise 3.2" should "have the value I indicate here" in {
    val answer = 3
    val ExerciseVal = fpinscalamulti.List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      //   Cons(1, Cons(2, Cons(3, Cons(4, _)))) => 1 + 2
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(answer === ExerciseVal)
  }

}
