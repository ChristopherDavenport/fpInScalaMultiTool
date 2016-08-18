package fpinscalamulti.Vanilla.laziness

/**
  * Created by davenpcm on 7/24/16.
  */
trait Stream[+A] {
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
