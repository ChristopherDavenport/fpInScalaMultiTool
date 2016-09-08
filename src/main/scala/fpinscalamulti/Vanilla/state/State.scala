package fpinscalamulti.Vanilla.state

/**
  * Created by davenpcm on 8/21/16.
  */
case class State[S, +A](run: S => (A,S)){
  import State._
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](that: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap{ a => that.map{ b => f(a, b)}}


}

object State{
  def unit[S, A](a: A): State[S, A] = State( state => ( a, state) )


}
