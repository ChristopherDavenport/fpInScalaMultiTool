package fpinscalamulti.Vanilla.state

/**
  * Created by davenpcm on 8/21/16.
  */
case class State[S, +A](run: S => (A,S)){
  import State._


  /**
    * This pattern takes the state, runs it to the new context. with the same State Type.
    * @param f The function that generates the new state and transforms the value
    * @tparam B The new type inside the new State
    * @return  A new State
    */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  /**
    * Change the inner value without effecting the state.
    * @param f The transforming function
    * @tparam B The type transforming into
    * @return
    */
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](that: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap{ a => that.map{ b => f(a, b)}}



}

object State{

  /**
    * Our First Implementation Of State. Takes any value and elevates it into a state context
    * without effecting the state.
    * @param a The value to put into the State context
    * @tparam S The type of the State
    * @tparam A The type of the value to elevate to the state context
    * @return A State of the given value
    */
  def unit[S, A](a: A): State[S, A] = State( state => ( a, state) )

  type Rand[A] = State[RNG, A]

  val randInt: Rand[Int] = State(rng => rng.nextInt)
  val randNonNegativeInt: Rand[Int] = randInt.map{
    case lessThanZero if lessThanZero < 0 => 1 - lessThanZero
    case works => works
  }
  val randDouble: Rand[Double] = randNonNegativeInt.map(int => int.toDouble / Int.MaxValue.toDouble )

  val randInts: Int => Rand[List[Int]] = count => List.fill(count)(randInt).sequence

  implicit class listStateOps[S,A](l: List[State[S, A]]){

    def sequence: State[S, List[A]] = {
      l.reverse.foldLeft(State.unit[S,List[A]](List[A]()))((acc, next) => next.map2(acc)(_ :: _))
    }

  }



  /**
    * Like Unit but rather then setting the secondary type we are setting the
    * state and discarding values. This lifts a state context rather than the value
    * context.
    * @param s The state
    * @tparam S The type of the state
    * @return
    */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def get[S]: State[S, S]= State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


  /**
    * Rules
    * 1) A machine thats out of candy ignores all inputs
    * 2) Inserting a coin into a locked machine will cause it to unlock
    * 3) Turn the knob on an unlocked machine will cause it to dispense candy
    *    and become locked
    * 4) Turning the knob on a locked machine does nothing
    * 5) Inserting a coin into an unlocked machine does nothing
    */

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Candies(value: Int) extends AnyVal{
//    def +(int: Int) = Candies(value + int)
//    def +(candies: Candies) = Candies(value + candies.value)
    def -(int: Int) = Candies(value - int)
//    def -(candies: Candies) = Candies(value - candies.value)
  }
  case class Coins(value: Int) extends AnyVal{
    def +(int: Int) = Coins(value + int)
//    def +(coins: Coins) = Coins(value + coins.value)
//    def -(int: Int) = Coins(value - int)
//    def -(coins: Coins) = Coins(value - coins.value)
  }


  case class Machine(locked: Boolean, candies: Candies, coins: Coins)

  /**
    * I have intentionally made this explicit in functions to make the reasoning clear
    * this needs an input and generates a function which takes a machine and give us back a
    * machine.
    */
  val modifyMachine : Input => Machine => Machine =
    input => machine => {

    (input, machine) match {
      case (_,    Machine(_, Candies(0), _)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
    }
  }


  /**
    * So Modify[Machine] takes a function Machine => Machine => State[Machine, Unit]
    *
    * modifyMachine takes a function Input => Machine => Machine
    *
    * inputs.map then Creates a List[State[Machine, Unit]].
    * So essentially List[Input] => List[State[Machine, Unit]].
    *
    * Sequence then takes the previous state through each State to generate a single State[S, List[Unit]].
    * List[State[Machine, Unit]] => State[Machine, List[Unit]].
    *
    * Finally get is called which pulls the internal state to the value side.
    * which is transformed to only the coins and candies.
    *
    * Applying A Machine to this state will return ((Coins, Candies), Machine)
    *
    * @param inputs A list of inputs to change the machine
    * @return A State that will output the final count of candies and
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Coins, Candies)] = for {
    _ <- inputs.map( modify[Machine] _ compose  modifyMachine ).sequence
    s <- get
  } yield (s.coins, s.candies)




}
