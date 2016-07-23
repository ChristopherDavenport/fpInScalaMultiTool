package io.christopherdavenport.fpinscalamultitool

/**
  * Created by davenpcm on 7/5/16.
  */
trait Option[+A] {
  /**
    * Exercise 4.1
    * This function transforms an Option if it is a some to some other option, and does nothing
    * if it is a None
    * @param f The transformation functions from A => B
    * @tparam B The new type of the option
    * @return An Option of Type B
    */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /**
    * Exercise 4.1
    * This is how we can extract information out of an Option. If there is a value within the option it is returned
    * or we can replace it with a placeholder value, or another valid piece of information that tells us it did
    * not exist.
    * @param default This is the return if the Option is None
    * @tparam B The type of the Option
    * @return Either the default return if the option is None or the value within the Option if it is not.
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  /**
    * The flatMap method serves to chain multiple failable operations. It takes another function which can transform
    * the initial type of the Option to an Option and if it is a some applies that operation, and if it does not
    * it returns None. This is done by creating an Option of an Option, then getOrElse with the Return Being None.
    * Meaning that if the outer Option was already None it is flattened, and if it was Some(Some(_)) that to was
    * flattened.
    * @param f A function which takes the type of the Option to another Option
    * @tparam B The end result of the function and the option after the operation
    * @return An Option of Type B
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  /**
    * This is like getOrElse but stays within the category of Option. As if it is a some we return the original Option
    * and if it is not we return the Option given by ob.
    * @param ob The return if the Option is currently None
    * @tparam B The type of the Option
    * @return The option if it is some or ob if it is not.
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  /**
    * Simple filter that uses flatMap as the filtering operation. If the value satisfies the predicate return the
    * current option else change from Some to None
    * @param f the evaluation predicate
    * @return Either the same Option or None
    */
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{

  def apply[A](v: A): Option[A] = if (v != null) { Some(v) } else None

}