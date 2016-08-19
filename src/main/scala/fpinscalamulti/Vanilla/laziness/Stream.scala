package fpinscalamulti.Vanilla.laziness

/**
  * Created by davenpcm on 7/24/16.
  */
trait Stream[+A] {

  import Stream._

  /**
    * Example HeadOption which returns the Option of the Head if it exists or None if the Stream is Empty
    * @return An Option of Type A
    */
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
    * Exercise 5.1
    * This function strictly evaluates the entire Stream
    * @return A list of Type A
    */
  def toList: List[A] = this match {
    case Cons(h, t) =>
      val head = h()
      val tail = t()
      head :: tail.toList
    case Empty =>
      List[A]()
  }

  /**
    * From Example -> Stack Safe
    * def toList: List[A] = {
    *   AT annotation.tailrec
    *   def go(s: Stream[A], acc: List[A] = s match {
    *     case Conse(h,t) => go(t(), h():: acc)
    *     case _ => acc
    *   }
    *   go(this, List()).reverse
    * }
    *
    */

  /**
    * Exercise 5.2
    * This works on the Stream to take a certain number of elements. It uses the smart constructor constructor for
    * memoization, and then the tail continues through this process. Important to realize that the take method as a
    * result has already evaluated all values in the stream that were taken
    * @param i The number of elements from the stream to take
    * @return A new stream with a number of elements equal to or less than depending on the original Stream with the
    *         elements memoized.
    */
  def take(i: Int): Stream[A] = this match {
    case Cons(h, t) if i > 1  => cons(h(), t().take(i - 1))
    case Cons(h, _) if i == 1 =>  cons(h(), empty)
    case _ => empty
  }

  /**
    * Exercise 5.2
    * This works by ignoring the head argument and continuing to evaluate the tail to create a new list, this has
    * absolutely no evaluation as it merely gets the rest of the stream and continues on.  In the case i reaches
    * zero it  returns the current stream, still unevaluated.
    * @param i The number of values to drop
    * @return A stream with that many fewer element.
    */
  @annotation.tailrec
  final def drop(i: Int): Stream[A] = this match {
    case Cons(_, t) if i > 0 => t().drop(i -1)
    case _ => this
  }

  /**
    * Take while takes a conditional an applies it to the head of the list, if it is empty, hence the end, we return
    * whatever exists, if it meets the criteria we built the stream and continue evaluation on the tail. If it
    * does not satisfy the value we apply empty in the final place replacing any of the following values in the stream
    * without evaluation.
    *
    * Not stack safe..
    *
    * @param p The predicate to filter on
    * @return A stream consisting of elements from the front of the stream until a value fails to meet the predicate.
    */
  def takeWhile1(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile1(p))
    case Cons(_, _) => empty
  }

  /**
    * Example
    *
    * Since || is non strict in its second argument this only continues until if finds an element and then
    * the computation stops.
    *
    * @param p The predicate to look for
    * @return True if a value exists that meets the predicate, false if not.
    */
  def exists(p: A=> Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  /**
    *
    * @param z The value that is placed at the end of evaluation
    * @param f The function that takes the aggregrate from the right and give a B
    * @tparam B The returned value of the aggregation function
    * @return A type B based on what the aggregat function implemented
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
    * If all values are true then it is true, if it is not then it is false and short circuits immediately
    * @param p The predicate to search against
    * @return A boolean representing whether all values of the stream match the predicate.
    */
  def forAll(p: A=> Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  /**
    * Exercise 5.5
    * @param p
    * @return
    */
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream(): Stream[A]){
    (a, b) => if (p(a)) cons(a, b.takeWhile(p)) else empty
  }

  /**
    * Exercise 5.6
    * Looks at the first element, if its Empty stops Evaluation, else returns A as a Some
    * @return An Option depending on the first elements existence
    */
  def headOptionFoldR: Option[A] = foldRight(None: Option[A])((a, _) => if (isEmpty) None else Some(a))

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /**
    * Smart Constructor for creating a nonemepty stream.
    * @param hd The head of the non empty stream
    * @param tl The tail of the nonempty stream
    * @tparam A The type of the stream
    * @return A nonempty stream.
    */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
    * A smart constructor for creatign an empty stream of a particular type
    * We cache the head and tail as lazy values to avoid repeated evaluation
    *
    * @tparam A The type of the stream
    * @return An Empty Stream
    */
  def empty[A]: Stream[A] = Empty

  /**
    * A convenient variable-argument method for constructing a Stream from multiple elements.
    * @param as Variadic arguments to construct a stream
    * @tparam A The type of the stream
    * @return A Stream of type A
    */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
