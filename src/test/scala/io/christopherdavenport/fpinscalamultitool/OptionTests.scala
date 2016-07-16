package io.christopherdavenport.fpinscalamultitool

import org.scalatest.{FlatSpec, Matchers}
import io.christopherdavenport.fpinscalamultitool.{Option => option}
import io.christopherdavenport.fpinscalamultitool.{Some => some}
import io.christopherdavenport.fpinscalamultitool.{None => none}


/**
  * Created by davenpcm on 7/5/16.
  */
class OptionTests extends FlatSpec with Matchers{

  "apply" should "create a some if there is a value" in {
    val i = 1
    Option(i) should be (some(1))
  }

  it should "create a none with a null" in {
    val i = null
    Option(i) should be (None)
  }

  "map" should "return an option changed by a function" in {
    val f: Int => Int = _ + 1
    val o = some(1)
    o.map(f) should be (some(2))
  }
}
