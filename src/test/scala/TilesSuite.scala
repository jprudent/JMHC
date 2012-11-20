package org.liprudent.majiang

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.liprudent.majiang.Tiles._

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
      println(sorted)
      assert(sorted == List(b2,b2,b3,c1,c2,s1,s1),sorted)
    }
  }

  test("Chows finding") {
    new Hand1 {
      val hand1 = List(b1,b2,b3,b4)
      val chows1 = findChows(hand1)
      assert(chows1 == List(Chow(b1,b2,b3),Chow(b2,b3,b4)),chows1)

      val hand2 = List(b1,b2,b3,b1,b2)
      val chows2 = findChows(hand2)
      assert(chows2 == List(Chow(b1,b2,b3)),chows2)
    }
  }

  test("Pungs finding") {
    new Hand1 {
      val hand1 = List(b1,b3,b1,b4,b5,b4,b4,b1)
      val pungs1 = findPungs(hand1)
      assert(pungs1 == List(Pung(b1),Pung(b4)),pungs1)

      val hand2 = List(b1,b2,b3,b1)
      val pungs2 = findPungs(hand2)
      assert(pungs2 == List(),pungs2)
    }
  }

}
