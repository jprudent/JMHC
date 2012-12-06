package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TilesSuite extends FunSuite {
  trait Hand1 {
    val b1 = Tile(Bamboo, 1)
    val b2 = Tile(Bamboo, 2)
    val b3 = Tile(Bamboo, 3)
    val b4 = Tile(Bamboo, 4)
    val b5 = Tile(Bamboo, 5)
    val b6 = Tile(Bamboo, 6)
    val b7 = Tile(Bamboo, 7)
    val b8 = Tile(Bamboo, 8)
    val b9 = Tile(Bamboo, 9)
    val c1 = Tile(Character, 1)
    val c2 = Tile(Character, 2)
    val s1 = Tile(Stone, 1)
  }
  
  test("Tile sorting") {
    new Hand1 {
      assert(b1.previousOf(b2))
      assert(!b2.previousOf(b1))
      assert(!c2.previousOf(b3))
      assert(b1.sameFamily(b2))
      assert(!c1.sameFamily(b1))
      
      val sorted =     List(b3,b2,s1,c1,s1,c2,b2).sorted
      assert(sorted == List(b2,b2,b3,c1,c2,s1,s1),sorted)
    }
  }

  test("Hand creation") {
    new Hand1 {
      val expected = List((b1,2),(b2,4),(b3,1))

      val lst = List(b1,b2,b3,b1,b2,b2,b2)
      val actual = Hand(lst).hand

      assert(actual == expected, actual + " instead of " + expected)
    }
  }

  test("Split by family") {
    new Hand1 {
      val hand1 = Hand(List(b2,c1,b1,s1,c2))

      val families: List[List[TileOccurence]] = hand1.splitByFamily
      assert(families == List(List((b1,1),(b2,1)), List((c1,1),(c2,1)), List((s1,1))),families)

    }
  }

  test("remove when multiple") {
    new Hand1 {
      val hand = Hand(List(b1,b2,b3,b1,b2,b2,b2))
      val actual = hand.remove(b1)
      val expected = Hand(List(/*X*/b2,b3,b1,b2,b2,b2))
      assert(actual == expected, actual +"instead of \n" + expected)
    }
  }

  test("remove when single") {
    new Hand1 {
      val hand = Hand(List(b1,b2,b3,b1,b2,b2,b2))
      val actual = hand.remove(b3)
      val expected = Hand(List(b1,b2,/*X*/b1,b2,b2,b2))
      assert(actual == expected, actual +"instead of \n" + expected)
    }
  }

  test("remove inner when single") {
    new Hand1 {
      val hand = Hand(Nil,2)
      val tiles = List((b1,1), (b2,4), (b3,1))
      val actual = hand.remove(b1,tiles)
      val expected = List((b2,4), (b3,1))
      assert(actual == expected, actual +"instead of \n" + expected)
    }
  }

  test("remove several") {
    new Hand1 {
      val hand = Hand(Nil,0xF00)
      val tiles = List((b1,1), (b2,4), (b3,1))
      val actual = hand.remove(List(b1,b3),tiles)
      val expected = List((b2,4))
      assert(actual == expected, actual +"instead of \n" + expected)
    }
  }

  test("Find suits") {
    new Hand1 {
      val hand1 = Hand(List(b1,b2,b3,b1,b2,b2,b2))
      val actual: List[Suit] = hand1.findSuits(hand1.hand)
      val expected = List(List(b1,b2,b3),List(b1,b2), List(b2), List(b2))

      assert(actual == expected, actual)


    }
  }

  test("Chows finding") {
    new Hand1 {
      val hand1 = Hand(List(b1,b2,b3,b4))
      val chows1 = hand1.findChows
      assert(chows1 == List(Chow(b1,b2,b3),Chow(b2,b3,b4)),chows1)

      val hand2 = Hand(List(b1,b2,b3,b1,b2))
      val chows2 = hand2.findChows
      assert(chows2 == List(Chow(b1,b2,b3)),chows2)
    }
  }

  test("Pungs finding") {
    new Hand1 {
      val hand1 = Hand(List(b1,b3,b1,b4,b5,b4,b4,b1))
      val pungs1 = hand1.findPungs
      assert(pungs1 == List(Pung(b1),Pung(b4)),pungs1)

      val hand2 = Hand(List(b1,b2,b3,b1))
      val pungs2 = hand2.findPungs
      assert(pungs2 == List(),pungs2)
    }
  }

}
