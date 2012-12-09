package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.liprudent.majiang.tiles.Tile._
import org.liprudent.majiang.tiles.Types.TileOccurence
import org.liprudent.majiang.figures._

@RunWith(classOf[JUnitRunner])
class TilesSuite extends FunSuite {

  test("Tile sorting") {
    assert(b1.previousOf(b2))
    assert(!b2.previousOf(b1))
    assert(!c2.previousOf(b3))
    assert(b1.sameFamily(b2))
    assert(!c1.sameFamily(b1))

    val sorted = List(we, ww, wn, ws, dr, dg, dw, b3, b2, s1, c1, s1, c2, b2).sorted
    assert(sorted == List(b2, b2, b3, c1, c2, s1, s1, we, wn, ww, ws, dr, dg, dw), sorted)
  }

  test("Hand creation") {
    val expected = List((b1, 2), (b2, 4), (b3, 1), (dw, 2))

    val lst = List(b1, b2, b3, b1, dw, dw, b2, b2, b2)
    val actual = Hand(lst).hand

    assert(actual == expected, actual + " instead of " + expected)
  }

  test("Split by family") {
    val hand1 = Hand(List(b2, c1, b1, s1, c2, dr, dr, we, ww))

    val families: List[List[TileOccurence]] = hand1.splitByFamily
    assert(families == List(List((b1, 1), (b2, 1)), List((c1, 1), (c2, 1)), List((s1, 1)),
      List((we, 1)), List((ww, 1)), List((dr, 2))), families)

  }

  test("remove when multiple") {
    val hand = Hand(List(b1, b2, b3, b1, b2, b2, b2))
    val actual = hand.remove(b1)
    val expected = Hand(List(/*X*/ b2, b3, b1, b2, b2, b2))
    assert(actual == expected, actual + "instead of \n" + expected)
  }

  test("remove when single") {
    val hand = Hand(List(b1, b2, b3, b1, b2, b2, b2))
    val actual = hand.remove(b3)
    val expected = Hand(List(b1, b2, /*X*/ b1, b2, b2, b2))
    assert(actual == expected, actual + "instead of \n" + expected)
  }

  test("remove inner when single") {
    val hand = Hand(List[Tile]())
    val tiles = List((b1, 1), (b2, 4), (b3, 1))
    val actual = hand.remove(b1, tiles)
    val expected = List((b2, 4), (b3, 1))
    assert(actual == expected, actual + "instead of \n" + expected)
  }

  test("remove several") {
    val hand = Hand(List[Tile]())
    val tiles = List((b1, 1), (b2, 4), (b3, 1))
    val actual = hand.remove(List(b1, b3), tiles)
    val expected = List((b2, 4))
    assert(actual == expected, actual + "instead of \n" + expected)
  }

  test("Find suits") {
    val hand1 = Hand(List(b1, b2, b3, b1, b2, b2, b2, we, wn, dg, dg))
    val actual: List[Suit] = hand1.findSuits(hand1.hand)
    val expected = List(List(b1, b2, b3), List(b1, b2), List(b2), List(b2), List(we), List(wn), List(dg), List(dg))

    assert(actual == expected, actual)


  }

  test("findSubSuitsIndices") {
    val hand1 = Hand(List(b1, b2, b3, b4))
    val expected = List(List(0, 1, 2), List(1, 2, 3), List(2, 3, 4))
    val actual = hand1.findSubSuitsIndices(5, 3)
    assert(actual == expected, actual)
  }

  test("listOf") {
    val hand1 = Hand(List(b1, b2, b3, b4))
    val expected = List(List(b1, b2, b3), List(b2, b3, b4))
    val actual = hand1.listsOf(3, List(List(b1, b2, b3, b4)))
    assert(actual == expected, actual)
  }

  test("Chow sorting") {
    val expected = List(Chow(b6, b7, b8), Chow(c6, c7, c8), Chow(s2, s3, s4), Chow(s6, s7, s8))
    val actual = expected.reverse.sorted(OrdChow)
    assert(actual === expected)
  }

  test("Chows finding") {
    val hand1 = Hand(List(b1, b2, b3, b4, we))
    val chows1 = hand1.findChows
    assert(chows1 == List(Chow(b1, b2, b3), Chow(b2, b3, b4)), chows1)

    val hand2 = Hand(List(b1, b2, b3, b1, b2, ww, ws))
    val chows2 = hand2.findChows
    assert(chows2 == List(Chow(b1, b2, b3)), chows2)

    val hand3 = Hand(List(b1, b3, b4, dr, dg))
    val chows3 = hand3.findChows
    assert(chows3 == List(), chows3)

    val hand4 = Hand(List(c1, b3, b2, c2, c3, c7, c8, c9, s1, b1, b1, b3, b2))
    val chows4 = hand4.findChows
    assert(chows4 == List(Chow(b1, b2, b3), Chow(b1, b2, b3), Chow(c1, c2, c3), Chow(c7, c8, c9)), chows4)
  }

  test("Dui finding") {
    val hand1 = Hand(List(b1, b1, dr, dr, dr, ww, ww, ww, ww))
    val duis1 = hand1.findDuis
    assert(duis1 == List(Dui(b1), Dui(ww), Dui(ww), Dui(dr)), duis1)
  }

  test("Pungs finding") {
    val hand1 = Hand(List(b1, b3, b1, b4, b5, b4, b4, b1))
    val pungs1 = hand1.findPungs
    assert(pungs1 == List(Pung(b1), Pung(b4)), pungs1)

    val hand2 = Hand(List(b1, b2, b3, b1))
    val pungs2 = hand2.findPungs
    assert(pungs2 == List(), pungs2)
  }

}
