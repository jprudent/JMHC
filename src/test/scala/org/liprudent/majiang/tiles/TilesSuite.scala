package org.liprudent.majiang.tiles

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Tile._
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

  test("Figures ordering") {
    val expected = List(Pung(b1), Pung(b2), Pung(b3), Chow(b1, b2, b3), Chow(b2, b3, b4), Chow(b4, b5, b6), Dui(b1), Dui(b2), Dui(b3))
    val actual = expected.reverse.sorted(OrdFigure)
    assert(actual === expected, actual)
  }

  test("simple Figures ordering") {
    val expected = List(Pung(b1), Chow(b1, b2, b3), Dui(b2))
    val actual = List(Chow(b1, b2, b3), Pung(b1), Dui(b2)).sorted(OrdFigure)
    assert(actual === expected)
  }

  test("TileSet method exists") {
    val ts = TileSet(List(c1, c4, c7, b2, b5, b8, s3, s6, s9))
    assert(ts.exists(_ == c1))
    assert(ts.exists(_ == c4))
    assert(ts.exists(_ == s3))

  }

  test("Tile matching by short name") {
    assert(Tile("we") === we)
    assert(Tile("wn") === wn)
    assert(Tile("ww") === ww)
    assert(Tile("ws") === ws)
    assert(Tile("dr") === dr)
    assert(Tile("c9") === c9)
  }

}
