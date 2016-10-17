package fpinscalamulti.Vanilla.state

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by davenpcm on 10/16/16.
  */
class StateTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import State._



  "unit" should "elevate a value to a context of the state" in {
    forAll("state", "value") { (s: Int, v: Int) =>
      val (value, state) = State.unit(v).run(s)
      value should be (v)
      state should be (s)
    }
  }

  "map" should "return a new State based on the original" in {
    forAll("seed"){(s: Long) =>
      val rngInit = RNG.SimpleRNG(s)
      val int: Rand[Int] = State(rng => rng.nextInt)
      val (value, state) = int.map(i => i - i).run(rngInit)
      value should be (0)

    }
  }

  "map2" should "combine 2 states into a new State" in {
    forAll("seed", "value"){(s: Long, i: Int) =>
      val rngInit = RNG.SimpleRNG(s)
      val int: Rand[Int] = State(rng => rng.nextInt)

      val (val1, rng1) = int.run(rngInit)
      val (val2, rng2) = int.run(rng1)


      val (value, state) = int.map2(int)(_ - _).run(rngInit)
      value should be (val1 - val2)
      state should be (rng2)
    }
  }

  "randInt" should "generate a random Integer" in {
    forAll("seed"){l: Long =>

      val (int, _ ) = randInt.run(RNG.SimpleRNG(l))
      int should be >= Int.MinValue
      int should be <= Int.MaxValue
    }
  }

  "randNonNegativeInt" should "generate a random positive integer" in {
    forAll("seed"){l: Long =>
      val (int, _ ) = randNonNegativeInt.run(RNG.SimpleRNG(l))
      int should be >= 0
      int should be <= Int.MaxValue
    }
  }

  "randDouble" should "generate a random double between zero and 1" in {
    forAll("seed"){l: Long =>
      val (double, _) = randDouble.run(RNG.SimpleRNG(l))
      double should be >= 0D
      double should be <= 1D
    }
  }

  val positiveReasonableSizedInt = for {
    i <- Gen.choose(0, 1000)
  } yield i

  val arbLong = for {
    l <- Gen.choose(Long.MinValue, Long.MaxValue)
  } yield l

  "randInts" should "generate a List of Random Integers" in {
    forAll( arbLong, positiveReasonableSizedInt){(l: Long, count: Int) =>
      whenever(count >= 0) {
        val (list, _) = randInts(count).run(RNG.SimpleRNG(l))
        list.length should be(count)
      }
    }
  }

  /**
    * Rules
    * 1) A machine thats out of candy ignores all inputs
    * 2) Inserting a coin into a locked machine will cause it to unlock
    * 3) Turn the knob on an unlocked machine will cause it to dispense candy
    *    and become locked
    * 4) Turning the knob on a locked machine does nothing
    * 5) Inserting a coin into an unlocked machine does nothing
    */

  val listOfInputs = Gen.listOf(Gen.oneOf(Coin, Turn))

  val outOfCandyMachine = for {
    locked <- Gen.oneOf(true, false)
    candy <- Gen.const(0)
    coins <- Gen.choose(0, Int.MaxValue - 1)
  } yield Machine(locked, Candies(candy), Coins(coins))

  "simulateMachine" should "ignore all inputs on a machine thats out of candy" in {
    forAll(listOfInputs, outOfCandyMachine){(inputList: List[Input], machine: Machine) =>

      val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

      coins should be (machine.coins)
      candies should be (machine.candies)
      finMachine should be (machine)
    }
  }

  val lockedMachine = for {
    locked <- Gen.const(true)
    candy <- Gen.choose(0, Int.MaxValue - 1)
    coins <- Gen.choose(0, Int.MaxValue - 1)
  } yield Machine(locked, Candies(candy), Coins(coins))

  it should "do nothing if you turn the knob on a locked machine" in {
    forAll(Gen.listOf(Gen.const(Turn)), lockedMachine){(inputList: List[Input], machine: Machine) =>

      val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

      coins should be (machine.coins)
      candies should be (machine.candies)
      finMachine should be (machine)
    }
  }

  val unlockedMachine = for {
    locked <- Gen.const(false)
    candy <- Gen.choose(0, Int.MaxValue - 1)
    coins <- Gen.choose(0, Int.MaxValue - 1)
  } yield Machine(locked, Candies(candy), Coins(coins))

  it should "do nothing if you insert a coin on an unlocked machine" in {
    forAll(Gen.listOf(Gen.const(Coin)), unlockedMachine){(inputList: List[Input], machine: Machine) =>

      val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

      coins should be (machine.coins)
      candies should be (machine.candies)
      finMachine should be (machine)
    }
  }

  it should "unlock when you place a coin in a locked machine" in {
    forAll(lockedMachine){ machine: Machine =>
      val inputList = List(Coin)

      val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

      finMachine.locked should be (!machine.locked)
      coins should be (machine.coins + 1)
      candies should be (machine.candies)
      finMachine should be (machine.copy(locked = false, coins = machine.coins + 1))
    }
  }

  it should "dispense candy and unlock if you apply a turn to an unlocked Machine" in {
    forAll(unlockedMachine){ machine: Machine =>
      val inputList = List(Turn)

      val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

      finMachine.locked should be (!machine.locked)
      coins should be (machine.coins )
      candies should be (machine.candies - 1)
      finMachine should be (machine.copy(locked = true, candies = machine.candies - 1))
    }
  }

  // For example if the input machine has 10 coins and 5 candies and a
  // total of 4 candies are succesfully bouth the output should
  // be (14, 1)
  it should "follow the example listed in the book" in {
    val machine = Machine(locked = true, Candies(5), Coins(10))
    val inputList1= List(Coin, Turn)
    val ((coins1, candies1), finMachine1) = simulateMachine(inputList1).run(machine)
    coins1 should be (Coins(11))
    candies1 should be (Candies(4))

    val inputList = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val ((coins, candies), finMachine) = simulateMachine(inputList).run(machine)

    coins should be (Coins(14))
    candies should be (Candies(1))

  }





}
