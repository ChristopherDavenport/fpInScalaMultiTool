package io.christopherdavenport.fpinscalamultitool

import org.scalatest.{FlatSpec, Matchers}
import Tree._
/**
  * Created by davenpcm on 7/4/16.
  */
class TreeTests extends FlatSpec with Matchers{

  "size" should "return the total number of leaves and nodes" in {
    val t = Branch(Leaf(1), Leaf(1))
    t.size should be (3)
  }

  "maximum" should "return the largest element in a tree of Int" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(7))), Leaf(4))
    maximum(t) should be (7)
  }

  "depth" should "return the depth of the tree" in {
    val t = Branch(Leaf(1), Leaf(1))

    t.depth should be (2)
  }

  it should "return correctly for unbalanced trees" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(7))), Leaf(4))
    t.depth should be (4)
  }

  "map" should "transform a tree by a given function" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f : Int => Int = _ + 1

    t.map(f) should be (Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
  }

  "maximumViaFold" should "return the largest element in a tree of Int" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(7))), Leaf(4))

    maximumViaFold(t) should be (7)
  }

  it should "return for singular values" in {
    val l = Leaf(8)

    maximumViaFold(l) should be (8)
  }

  "depthViaFold" should "return the depth of the tree" in {
    val t = Branch(Leaf(1), Leaf(1))

    depthViaFold(t) should be (2)
  }

  it should "return correctly for unbalanced trees" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(7))), Leaf(4))
    depthViaFold(t) should be (4)
  }

  "sizeViaFold" should "return the total number of leaves and nodes" in {
    val t = Branch(Leaf(1), Leaf(1))
    sizeViaFold(t) should be (3)
  }

  it should "work for more intricate Trees" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(7))), Leaf(4))
    sizeViaFold(t) should be (7)
  }

  "mapViaFold" should "transform a tree by a given function" in {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f : Int => Int = _ + 1

    mapViaFold(t)(f) should be (Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
  }

}
