package fpinscalamulti

/**
  * Created by davenpcm on 7/24/16.
  *
  * Disjoint Union of 2 Types
  *
  */
sealed trait Either[+E, +A]{

  /**
    * Checks if the value is currently right
    * @return Boolean if value is right true, else false
    */
  def isRight: Boolean =  this match {
    case Right(a) => true
    case Left(e)  => false
  }

  /**
    * Checks is value is currently left
    * @return True if value is right, since there are only 2 types we can assert !right is always left
    */
  def isLeft: Boolean = !isRight

  /**
    * Exercise 4.5
    * map
    * flatMap
    * orElse
    * map2
    *
    */

  /**
    * The function map operates on the right hand side, if it has a value it maps it to the new domain
    * if it does not then it proliferates the left
    * @param f The function that transforms the value of the Right
    * @tparam B The type of the right returned
    * @return A new either with the right of type B
    */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => Right(f(x))
  }

  /**
    * This takes a function that might itself switch to a left and returns an either.
    *
    * Their is not a good extractor for an either as we don't know what the type of Left Might be or what
    * should be there, so instead we proliferate the left and map from the internals of right rather than mapping
    * and extracting as was done with option.
    *
    * @param f the function that might switch to a left
    * @tparam EE The type on the left
    * @tparam B The type of the Right
    * @return A new Either of type B
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => f(x)
  }

  /**
    * Same as on flatMap except instead of proliferating the left we proliferate the right and substitute the
    * b if it is left
    * @param b The placeholder value to put in place if it is left
    * @tparam EE The type of the Left
    * @tparam B The type of the Right
    * @return Some Either as b could also be a Left
    */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(v) => Right(v)
  }

  /**
    * This maps 2 eithers together with a function. We utilize flatmap to squash the two layers.
    * @param that the second either
    * @param f The function that combines the two rights of the eithers
    * @tparam EE The type of the Left shared between the two eithers
    * @tparam B the Type of the Second Eithers Right
    * @tparam C The Return Type of the Right of the Either
    * @return An either representing the two right values combined with the function or either left
    *         between the two with the Left of the first value erasing the information of the Left information
    *         of the second.
    */
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    value <- this
    otherValue <- that
  } yield f(value, otherValue)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}
}