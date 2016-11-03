package fpinscalamulti.Vanilla.datastructures

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import java.util.Calendar
import scala.Ordering

/**
  * Created by davenpcm on 11/3/16.
  */
class LRUCacheTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  var longVar = 0L
  def currentLong(): Long = {
    longVar += 1
    longVar
  }

  "LRUCache" should "take only up to a specific number of elements" in {
    val e = LRUCache[String, Int, Long](3)(currentLong)
    val lru1 = e.set("Awesome" -> 1)
    val lru2 = lru1.set("Cool" -> 2)
    val lru3 = lru2.set("Amazing" -> 3)
    val lru4 = lru3.set("Fantastic" -> 4)

    lru4.size should be (3)

  }

  it should "be able to be initialized empty" in {
    val e = LRUCache[String, Int, Long](3)(currentLong)
    e.size should be (0)
  }

  it should "have removed the first element if it is size 3 and set 4 elements" in {
    val e = LRUCache[String, Int, Long](3)(currentLong)
    val lru1 = e.set("Awesome" -> 1)
    val lru2 = lru1.set("Cool" -> 2)
    val lru3 = lru2.set("Amazing" -> 3)
    val lru4 = lru3.set("Fantastic" -> 4)

    val (result, lru5) = lru4.get("Awesome")

    result should be (Option.empty[Int])
  }

  it should "change the remove order with get operations" in {
    val e = LRUCache[String, Int, Long](3)(currentLong)
    val lru1 = e.set("Awesome" -> 1)
    val lru2 = lru1.set("Cool" -> 2)
    val lru3 = lru2.set("Amazing" -> 3)
    val (opt, lru4) = lru3.get("Awesome")
    opt should be (Option(1))
    val lru5 = lru4.set("Fantastic" -> 4)

    val (result, lru6) = lru5.get("Awesome")
    val (result2, lru7) = lru6.get("Cool")

    result should be (Option(1))
    result2 should be (Option.empty[Int])

  }



}
