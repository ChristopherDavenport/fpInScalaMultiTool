package fpinscalamulti

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

  /**
    * Returns whether the option is currently defined.
    * @return A Boolean representing whether the option is in the Some state or the None state
    */
  def isDefined: Boolean = this match {
    case Some(a) => true
    case None => false
  }

  /**
    * Returns whether the option is currently Empty. As there are 2 states if it is defined it is
    * empty why it is not defined.
    * @return A Boolean representing whether the Option is currently None
    */
  def isEmpty: Boolean = !isDefined




}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{

  def apply[A](v: A): Option[A] = if (v != null) { Some(v) } else None

  /**
    * Lift returns a function which maps None to None and applies f to the contents of Some.
    * f need not be aware of the Option type at all.
    * @param f The function to lift to operate on Options
    * @tparam A The original type and the type of the intake of the function
    * @tparam B The transformed type, the return type of the function
    * @return A function which transforms an option to another option
    */
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /**
    * Exercise 4.3
    * This takes two option values and a function that takes the internal type of
    * those two options and combines them to single option in the output.
    *
    * This is syntactic sugar for a.flatmap( case va => b.map( case vb => f(va, vb)))
    *
    * Since it uses flatmap on the first it removes the extra layer of Option that might
    * have been created and the second option maps to the final option that is returned.
    *
    * @param a The first option
    * @param b The second option
    * @param f The transformation function of the internal types
    * @tparam A The Type of th First Option and the first operand
    * @tparam B The Type of the Second Option and the second operand
    * @tparam C The internal type of the final option
    * @return An option of type C
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      va <- a
      vb <- b
    } yield f(va, vb)
  }

//  def sequenceWeak[A](a: List[Option[A]]): Option[List[A]] = {
//    if (a.forall(_.isDefined)) Some(a.map(_.getOrElse(new A))) else None
//  }

  /**
    * Exercise 4.4
    * Infinite Mapping to Create an Option Around a Map and If any are Not defined the next call will
    * change the sequence from Some to None
    * @param a This list of Options
    * @tparam A the type of the Options
    * @return An Option of a List of A
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x, y)(_ :: _))



}