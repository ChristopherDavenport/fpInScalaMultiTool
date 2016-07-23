package fpinscalamulti.Vanilla

/**
  * Created by davenpcm on 7/3/16.
  */
object Chapter3 extends App{

  /**
    * Exercise 3.1 - The Result will be 3 As List(1,2,3,4,5) expands to
    * Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    *
    * Answer is 3 as 1 + 2 == 3
    *
    * Proof of Result is in Tests
    *
    * val result = List(1,2,3,4,5) match {
    *    case Cons(x, Cons(2, Cons(4, _))) => x
    *    case Nil => 42
    *    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    *    //   Cons(1, Cons(2, Cons(3, Cons(4, _)))) => 1 + 2
    *    case Cons(h, t) => h + sum(t)
    *    case _ => 101
    *  }
    */

  /**
    * Exercise 3.2 - 3.6 Are Implemented In List.scala
    */


  /**
    * Exercise 3.7 -
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    */

  /**
    * Exercise 3.8 -
    *
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    *
    * It Functions the same as the apply Method as it cons until it is out of elements and then places the Nil element.
    * This is proven as a test in ListTests.scala where we assert the list outcome is equal to the original list.
    */

  /**
    * Exercise 3.9 - 3.24 are in List.scala
    * Exercise 3.25 - 3.29 are in Tree.scala
    */

}
