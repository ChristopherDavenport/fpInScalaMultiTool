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
    * @param p The predicate to take While
    * @return A Stream consisting of Elements that match the predicate from the head of the stream.
    */
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream(): Stream[A]){
    (a, b) => if (p(a)) cons(a, b.takeWhile(p)) else empty
  }

  /**
    * Exercise 5.6
    * Looks at the first element, if its Empty stops Evaluation, else returns A as a Some
    * @return An Option depending on the first elements existence
    */
  def headOptionFoldR: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  /**
    * Checks whether the Stream is Empty
    * @return True if the stream is Empty
    */
  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  /**
    * Checks whether a stream in non empty
    * @return True when the Stream has an element
    */
  def isDefined: Boolean = !isEmpty

  /**
    * Exercise 5.7
    *
    * Map is easy, as fold right mimics the cons structure so we only need to build a new stream applying
    * the function to each argument
    *
    * @param f The transformation function
    * @tparam B The type of the resulting Stream
    * @return A new Stream transformed by the function f
    */
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a,b) => cons(f(a), b))

  /**
    * Exercise 5.7
    *
    * This function advances by applying the predicate function and applying the value to the stream if it matches
    * the predicate and if not passes what has been built so far to the next step
    * @param p The predicate to make sure all elements match
    * @return A new stream consisting only of elements that match the predicate
    */
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b )

  /**
    * Exercise 5.7
    *
    * This function is a little fun as it uses the nature of foldright acting upon cons cells, basically it builds
    * the stream and when it hits the empty at the end the new stream is placed their but no evaluation if ever done
    * to the list
    * @param s The Secondary Stream
    * @tparam B The Type of the second stream
    * @return
    */
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a,b) => cons(a, b))

  /**
    * Here we use the append function to apply each value to its own stream and then append the next value lazily
    * @param f The function that transforms to a Stream
    * @tparam B The type of the new Stream
    * @return A new Stream of type B
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty:Stream[B])((a,b) => f(a).append(b))

  /**
    * Finds the first element in a stream that matches a predicate
    * @param p The predicate to look for
    * @return An Option that returns the first element that matches or None
    */
  def find(p: A => Boolean): Option[A] = filter(p).headOptionFoldR

  /**
    * Exercise 5.13
    *
    * This function works by taking the state of the current list as the parameter evaluating that state
    * and terminating on empty
    * @param f The transformation function
    * @tparam B The type being transformed into
    * @return A Stream of Type B
    */
  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this){
    case Cons(h, t) => Option((f(h()), t()))
    case _ => None
  }

  /**
    * Exercise 5.13
    *
    * The state carried is both the state of the Stream and the state of the unfolding take integer
    * When We are at take one we implement the empty and set to zero so None terminates the unfold.
    * Otherwise we take implementing backwards in the Stream till we reach 1 and any other value terminates
    * @param n The number of values to take from the stream
    * @return A Stream consisting on n values or less
    */
  def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)){
    case (Cons(h, _), 1) => Some(h(), (empty, 0))
    case (Cons(h, t), i) if i > 1 => Option((h(), (t(), n-1)))
    case _ => None
  }

  /**
    * Exercise 5.13
    *
    * The Only state of concern in this function is that of the evaluation of the function which happens
    * at each step if not it is terminated
    * @param p The predicate each value must satisfy
    * @return A Stream consisting of the first elements of the list which satisfy the predicate until one
    *         does not
    */
  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this){
    case Cons(h, t) if p(h()) => Option(h(), t())
    case _ => None
  }

  /**
    * Exercise 5.13
    *
    * Simple if both lists have elements they are tupled, otherwise the stream is terminated.
    * @param that the Stream to zip with
    * @tparam B The type of the other stream
    * @return A Stream of Tuples of A and B
    */
  def zipWith[B](that: Stream[B]): Stream[(A,B)] = Stream.unfold((this, that)){
    case (Cons(h1, t1), Cons(h2, t2)) => Option((h1(), h2()),(t1(), t2()))
    case _ => None
  }

  /**
    * Exercise 5.13
    *
    * This function combines the entirety of two streams.
    * @param that The other stream
    * @tparam B The type of the Other Stream
    * @return As the entirety is returned all values are options as one list may terminate earlier than the other,
    *         and all following values in the stream on that side will be none.
    */
  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, that)){
    case (Cons(h1, t1), Cons(h2, t2)) => Option((Option(h1()), Option(h2())),(t1(), t2()))
    case (Cons(h1, t1), Empty) => Option((Option(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Option((None, Option(h2())), (Empty, t2()))
    case _ => None
  }


  /**
    * Exercise 5.14
    *
    * Checks Whether A Stream Starts with another Stream
    * @param s The other stream
    * @tparam B The type of the other stream, that must be a supertype of A
    * @return A boolean whether or not it is true, the only result which I question which matches the author is that
    *         empty always is true. Except the other element doesnt Start with Empty. So I have left it with the intent
    *         of the author however a quick check of s would fix it.
    */
  def startsWith[B >: A](s: Stream[B]): Boolean  = {
    zipAll(s).takeWhile(_._2.isDefined).forAll{ case (h1, h2) => h1 == h2}
  }

  /**
    * Exercise 5.15
    *
    * Returns all the tails of this list so each stream as an element back from the first
    * @return A stream of Streams
    */
  def tails: Stream[Stream[A]] = Stream.unfold(this){
    case Cons(h, t) =>
      Option(
      (cons(h(), t()), t())
    )
    case Empty => None
  }.append(Stream(empty))

  /**
    * Checks whether a given subsequence is in the stream.
    * @param s The other stream
    * @tparam B The type of the other stream which much by a supertype of A
    * @return A boolean representing the presence lack of presence of the sequence.
    */
  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails.exists(_.startsWith(s))

  /**
    * Exercise 5.16
    *
    * Tails and foldRight combined. So we combined them to get the correct result
    * @param z The Right Value
    * @param f The transformation function
    * @tparam B the result type
    * @return A Stream of Type B containing all intermediate results
    */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.mapUnfold(_.foldRight(z)(f))

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

  /**
    * Exercise 5.8
    *
    * This function recurses on itself to build a constant Stream of the element given to it
    * @param a The element to repeate
    * @tparam A The type of the Element
    * @return An infinite recursive Stream
    */
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, Stream.constant(a))
  }

  /**
    * Exercise 5.9
    *
    * Takes the value of n and increases by 1 infinitely on itself
    * @param n The starting integer
    * @return An infinite stream
    */
  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

  /**
    * Exercise 5.10
    * Uses the internal function to add first, second, and then stream the result of the two
    * @return An infinite stream of fibonacci numbers
    */
  def fibs: Stream[Int] = {
    def adder(last:Int, current: Int): Stream[Int] = Stream.cons(last, adder(current, last+current))
    adder(0,1)
  }

  /**
    * Exercise 5.11
    *
    * Generic Unfold Operation that builds streams.
    * Uses the result of the function to terminate or continue
    * @param z The initial state
    * @param f Function for producing the next state and next value in the generated stream
    * @tparam A The Value Type
    * @tparam S The State Type
    * @return A Stream Of Values
    */
  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(next) => Stream.cons(next._1, Stream.unfold(next._2)(f) )
    case None => empty[A]
  }

  /**
    * Exercise 5.12
    *
    * The state value is a tuple consisting of the element to be added and the element that is the next element.
    * Then In the second value of the state returned we return the addition of the two to create the infinite stream
    * @return
    */
  def fibsUnfold : Stream[Int] = Stream.unfold((0,1))(init => Option( ( init._1 , (init._2, init._1 + init._2) )))

  def fromUnfold(n: Int): Stream[Int] = Stream.unfold(n)(init => Option(init, init+1))

  def constantUnfold[A](a: A): Stream[A] = Stream.unfold(a)(a => Option(a, a))

}
